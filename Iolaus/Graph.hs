-- Copyright (C) 2009 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP, PatternGuards #-}

module Iolaus.Graph ( putGraph ) where

import Data.List ( nub, delete, (\\), intersect )
import Data.Maybe ( isJust, fromJust, catMaybes, maybeToList )

import Iolaus.Arguments ( Flag(Graph,Count,ShowMerges,MaxC) )
import Iolaus.Sealed ( Sealed(Sealed), unseal )
import Iolaus.Colors ( Color, resetCode, colorCode, rainbow )

import Git.Plumbing ( Hash, Commit, catCommit, myMessage, myParents )
import Git.Helpers ( showCommit )
import Git.Dag ( cauterizeHeads, parents )

data Spot = AtHome { homeIs :: Maybe (Sealed (Hash Commit)),
                     overlapping :: [Sealed (Hash Commit)] }
          | Away { homeIs :: Maybe (Sealed (Hash Commit)),
                   overlapping :: [Sealed (Hash Commit)] }
            deriving ( Show, Eq )

data GraphState = GS { allSpots :: [Spot],
                       gropts :: [Flag],
                       count :: Int,
                       colors :: [(Sealed (Hash Commit), Color)] }
                  deriving ( Show )

delo :: Sealed (Hash Commit) -> Spot -> Spot
delo h s = s { overlapping = delete h $ overlapping s }
addo :: Sealed (Hash Commit) -> Spot -> Spot
addo h s | Just h == homeIs s = AtHome (Just h) (overlapping s)
         | otherwise = s { overlapping = h : delete h (overlapping s) }

evolveSpots :: [Spot] -> [Spot]
evolveSpots = esHelper []

esHelper :: [Sealed (Hash Commit)] -> [Spot] -> [Spot]
esHelper _ [] = []
esHelper _ [x] = [x]
esHelper past (a:b)
    | Just h <- homeIs a,
      h `elem` overlapping a =
          esHelper (past++[h])
                   (AtHome (homeIs a) (delete h $ overlapping a) : b)
esHelper past (a:b:c)
    | o:_ <- filter (`elem` overlapping b) (past++maybe [] (:[]) (homeIs a)) =
             addo o a : esHelper (past++maybe [] (:[]) (homeIs a))
                                 (delo o b : c)
    | o:_ <- filter (`elem` catMaybes (map homeIs (b:c))) $ overlapping a =
             case esHelper (past++maybe [] (:[]) (homeIs a)) (b:c) of
               b':c' -> delo o a : addo o b' : c'
               [] -> error "sagdsdg"
esHelper past (a:c) = a : esHelper (past++maybe [] (:[]) (homeIs a)) c

data G a = G (GraphState -> IO (GraphState, a))
instance Monad G where
    G a >>= f = G $ \s -> do (s',x) <- a s
                             case f x of
                               G b -> b s'
    G a >> G b = G $ \s -> do (s',_) <- a s
                              b s'
    fail e = G $ const $ fail e
    return a = G $ \s -> return (s,a)

runG :: [Flag] -> G () -> IO ()
runG opts (G f) = do f (GS [] opts 0 []); return ()

io :: IO a -> G a
io f = G $ \s -> do x <- f
                    return (s,x)

putGenS :: Maybe (Sealed (Hash Commit)) -> String
        -> GraphState -> IO (GraphState, ())
putGenS mn l s = do let newspots = evolveSpots (allSpots s)
                    if Graph `elem` gropts s
                      then putStrLn (mkpref (allSpots s) newspots++"  "++l)
                      else if Count `elem` gropts s
                           then return ()
                           else putStrLn l
                    --putStrLn $ "    mn is : "++show mn
                    --putStrLn $ "    lines are: "++unwords
                    --             (map (show  . homeIs) $ allSpots s)
                    --putStrLn $ "    spots are: "++show (allSpots s)
                    return (s { allSpots = newspots },())
    where draw h c = maybe [c] (\cc -> colorCode cc++c:resetCode) $
                     lookup h (colors s)
          mkpref (a : c ) (a' : c')
              | homeIs a == mn && isJust mn = draw (fromJust $ homeIs a) '*' ++
                                              betw (a:c) (a':c')
          mkpref (a@(AtHome (Just h)  _) : c )
                 (a'@(AtHome (Just h') _) : c')
              | h == h' = draw h '|' ++ betw (a:c) (a':c')
          mkpref (a : b : c )
                 (a'@(AtHome (Just h) _) : b': c') =
                 draw h '.' ++ betw (a:b:c) (a':b':c')
          mkpref (a : c )
                 (a' : c') =
                 case overlapping a `intersect` overlapping a' of
                   o:_ -> draw o 'I' ++ betw (a:c) (a':c')
                   [] -> case overlapping a' of
                           [o] -> draw o '_' ++ betw (a:c) (a':c')
                           _ -> ' ' : betw (a:c) (a':c')
          mkpref [] [] = ""
          mkpref _ _ = "XXX"
          betw (a : b : c )
               (a' : b': c')
              | Just h <- homeIs a',
                h `elem` overlapping b && h `notElem` overlapping b' =
                  draw h '/' ++ mkpref (b:c) (b':c')
              | Just h <- homeIs b',
                h `elem` overlapping a && h `notElem` overlapping a' =
                  draw (fromJust $ homeIs b') '\\' ++ mkpref (b:c) (b':c')
              | o:_ <- overlapping a `intersect` overlapping b' =
                    draw o '\\' ++ mkpref (b:c) (b':c')
              | o:_ <-  overlapping a' `intersect` overlapping b =
                    draw o '/' ++ mkpref (b:c) (b':c')
              | otherwise = ' ' : mkpref (b:c) (b':c')
          betw a a' = mkpref (drop 1 a) (drop 1 a')

putS :: String -> G ()
putS s | '\n' `elem` s = mapM_ putS $ lines s
putS l = G $ putGenS Nothing l

getParents :: G [Sealed (Hash Commit)]
getParents = G $ \s -> return (s, catMaybes $ map homeIs $ allSpots s)

getCount :: G Int
getCount = G $ \s -> return (s, count s)

rmParent :: Sealed (Hash Commit) -> G ()
rmParent p = G $ \s -> return (s { allSpots = map rmp $ allSpots s }, ())
    where rmp s | homeIs s == Just p
                    = s { homeIs = Nothing,
                          overlapping = filter (/= p) $ overlapping s }
          rmp s = s { overlapping = filter (/= p) $ overlapping s }

node :: Sealed (Hash Commit) -> [Sealed (Hash Commit)] -> String -> G ()
node n ps name = G addit
    where addit s | not $ null $ concatMap overlapping $ allSpots s =
                      do (s',_) <- putGenS Nothing "" s
                         addit s'
          addit s = do let s0 = if Just n `elem` map homeIs (allSpots s)
                                then s
                                else s { allSpots =
                                             filter (/= Away Nothing [])
                                                    (allSpots s)
                                                    ++[Away (Just n) []] }
                       (s',_) <- putGenS (Just n) name s0
                       let oldps = catMaybes $ map homeIs $ allSpots s'
                           newps = nub (concatMap treeit $ oldps++[n]++ps)
                           treeit x | x == n = ps
                                    | otherwise = [x]
                           newspots = zipWith mixspot
                                      (map (maybeToList . homeIs) (allSpots s')++
                                       repeat [])
                                      (map Just newps++
                                       take (length oldps-length newps)
                                            (repeat Nothing))
                           mixspot :: [Sealed (Hash Commit)]
                                   -> Maybe (Sealed (Hash Commit))
                                   -> Spot
                           mixspot os (Just nn)
                               | nn `elem` os = AtHome (Just nn) (delete nn os)
                           mixspot os nn
                               | n `elem` os = mixspot (reverse ps++delete n os) nn
                           mixspot mo Nothing = Away Nothing mo
                           mixspot mo (Just nn) = Away (Just nn) mo
                           cs = filter ((`elem` newps) . fst) $ colors s'
                           unused = filter (`notElem` map snd cs) rainbow
                           choices =
                               case lookup n (colors s') of
                                 Just cn -> cn : delete cn unused
                                 Nothing -> unused
                           cs' = zip (newps \\ oldps) choices ++ cs
                       return (GS newspots (gropts s) (count s+1) cs',())

putGraph :: [Flag] -> (Sealed (Hash Commit) -> Bool)
         -> [Sealed (Hash Commit)] -> IO ()
putGraph opts isok hs0 = runG opts $ putGr opts isok hs0

putGr :: [Flag] -> (Sealed (Hash Commit) -> Bool)
      -> [Sealed (Hash Commit)] -> G ()
putGr opts isok hs0 =
    do ps <- getParents
       case cauterizeHeads (ps++hs0) of
         [] -> stopit
         h:xs | not (isok h) -> do rmParent h
                                   putGr opts isok xs
         h:xs -> do let hs = filter (`elem` hs0) xs
                    pict <- io $ showCommit opts `unseal` h
                    let n:body = lines $ show pict
                    pars <- io $ interestingParents opts h
                    node h pars n
                    mapM_ putS body
                    cnt <- getCount
                    if null [c | MaxC c <- opts, c <= cnt]
                        then putGr opts isok hs
                        else stopit
    where stopit = if Count `elem` opts then getCount >>= (io . putStrLn . show)
                                        else return ()

interestingParents :: [Flag] -> Sealed (Hash Commit)
                   -> IO [Sealed (Hash Commit)]
interestingParents opts h = concat `fmap` mapM ip (parents `unseal` h)
    where ip (Sealed c) = do cc <- catCommit c
                             if take 5 (myMessage cc) == "Merge" &&
                                length (myParents cc) > 1 &&
                                ShowMerges `notElem` opts
                               then interestingParents opts (Sealed c)
                               else return [Sealed c]
