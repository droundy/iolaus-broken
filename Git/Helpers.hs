{-# LANGUAGE CPP #-}
#include "gadts.h"

module Git.Helpers ( test, testCommits, testMessage,
                     testPredicate, TestResult(..),
                     commitTreeNicely, verifyCommit,
                     revListHeads, revListHeadsHashes,
                     slurpTree, writeSlurpTree, touchedFiles,
                     simplifyParents, configDefaults,
                     diffCommit, mergeCommits,
                     showCommit ) where

import Prelude hiding ( catch )
import Control.Exception ( catch )
import Control.Monad ( when )
import System.Directory ( doesFileExist )
import System.Posix.Process ( nice )
#ifndef HAVE_REDIRECTS
import System.Cmd ( system )
#else
import System.Process.Redirects ( system )
#endif
import System.Exit ( ExitCode(..) )
import System.IO ( hPutStrLn )
import System.IO.Unsafe ( unsafeInterleaveIO )
import Data.List ( group, sort, nub, (\\) )
import Data.ByteString as B ( hPutStr )

import Git.Dag ( chokePoints, parents,
                 cauterizeHeads, iao, notIn )
import Git.Plumbing ( Hash, Tree, Commit, TreeEntry(..),
                      uname, committer, remoteHeads,
                      setConfig, unsetConfig, ConfigOption(Global, System),
                      catCommit, catCommitRaw, CommitEntry(..), formatRev,
                      commitTree, updateref, parseRev,
                      mkTree, hashObject, lsothers,
                      diffFiles, DiffOption( NameOnly ),
                      readTree, checkoutIndex,
                      heads, revList, revListHashes, RevListOption,
                      catTree, catBlob, catCommitTree )

import Iolaus.Global ( debugMessage )
import Iolaus.Flags ( Flag( Test, Build, Sign, Verify, VerifyAny,
                            RecordFor, Summary, Verbose,
                            CauterizeAllHeads, ShowHash, Nice,
                            ShowParents, GlobalConfig, SystemConfig,
                            ShowTested ) )
import Iolaus.FileName ( FileName, fp2fn )
import Iolaus.IO ( ExecutableBit(..) )
import Iolaus.SlurpDirectoryInternal
    ( Slurpy(..), SlurpyContents(..), empty_slurpy,
      slurpies_to_map, map_to_slurpies )
import Iolaus.Lock ( removeFileMayNotExist, withTempDir )
import Iolaus.RepoPath ( setCurrentDirectory, getCurrentDirectory, toFilePath )
import Iolaus.Diff ( diff )
import Iolaus.Patch ( Prim, commute, apply_to_slurpy, mergeNamed,
                      infopatch, invert, summarize, showContextPatch )
import Iolaus.Ordered ( FL(..), (:>)(..), mapFL_FL )
import Iolaus.Sealed ( Sealed(..), FlippedSeal(..), mapSealM, unseal )
import Iolaus.Printer ( Doc, empty, text, prefix, ($$) )
import Iolaus.Gpg ( signGPG, verifyGPG )

touchedFiles :: IO [FilePath]
touchedFiles =
    do x <- lsothers
       y <- diffFiles [NameOnly] []
       return (x++lines y)

commitTreeNicely :: [Flag] -> Hash Tree C(x) -> [Sealed (Hash Commit)] -> String
                 -> IO (Hash Commit C(x))
commitTreeNicely opts t hs0 msg =
    do let hs1 = cauterizeHeads hs0
       hs <- if length hs1 < 2
             then return hs1
             else do k <- hashObject (`hPutStrLn` show (sort hs1))
                     ((:[]) `fmap` parseRev ("refs/tested/"++show k))
                            `catch` (\_ -> return hs1)
       x <- commitTree t hs msg
       if Sign `elem` opts 
           then do raw <- catCommitRaw x
                   putStrLn ("signing: "++show raw)
                   raw' <- signGPG raw
                   putStrLn ("signed: "++show raw')
                   commitTree t hs (unlines $ drop 1 $ dropWhile (/= "") $
                                            lines raw')
           else return x

verifyCommit :: [Flag] -> Sealed (Hash Commit) -> IO ()
verifyCommit opts c
    | not $ null ([True | VerifyAny <- opts]++
                  [True | Verify _ <- opts]) =
        do x <- catCommitRaw `unseal` c
           debugMessage ("verifying: "++show x)
           ctr <- unseal myCommitter `fmap` mapSealM catCommit c
           verifyGPG opts (takeWhile (/= '>') ctr++">") x
           debugMessage $ "Commit "++show c++" verified..."
verifyCommit _ _ = return ()

testCommits :: [Flag] -> String -> [Sealed (Hash Commit)]
            -> IO (Maybe (Sealed (Hash Commit)))
testCommits opts msg hs0 =
    do let hs = sort $ cauterizeHeads hs0
       k <- hashObject (`hPutStrLn` show hs)
       mt <- (Just `fmap` parseRev ("refs/tested/"++show k))
             `catch` (\_ -> return Nothing)
       case mt of
         Just c -> return $ Just c
         Nothing ->
             do Sealed t <- mergeCommits hs
                x <- testPredicate opts t
                case x of
                  Pass -> do m <- testMessage opts
                             let msg' = case m of [] -> [msg]
                                                  _ -> msg:"":m
                             c <- commitTree t hs (unlines msg')
                             updateref ("refs/tested/"++show k) (Sealed c)
                                                                Nothing
                                 `catch` (\_ -> return ())
                             return $ if null m then Nothing
                                                else Just (Sealed c)
                  Fail -> fail "test failed"
                  Unresolved -> fail "build failed"

test :: [Flag] -> Hash Tree C(x) -> IO [String]
test opts t =
    do x <- testPredicate opts t
       case x of
         Pass -> testMessage opts
         Fail -> fail "test failed"
         Unresolved -> fail "build failed"

testMessage :: [Flag] -> IO [String]
testMessage opts | any (`elem` opts) [Build, Test] =
    do havet <- doesFileExist ".test"
       if not havet || Build `elem` opts
          then do haveb <- doesFileExist ".build"
                  if not haveb
                     then return []
                     else on "Built" `catch` \_ -> by "Built"
          else on "Tested" `catch` \_ -> by "Tested"
    where on t = do x <- uname
                    return ["",t++"-on: "++x]
          by t = do x <- committer
                    return ["",t++"-by: "++x]
testMessage _ = return []

data TestResult = Pass | Fail | Unresolved
                  deriving ( Show, Eq )

testPredicate :: [Flag] -> Hash Tree C(x) -> IO TestResult
testPredicate opts t =
 do msg <- testMessage opts
    here <- getCurrentDirectory
    if null msg
     then return Pass
     else withTempDir "testing" $ \tdir -> do
       setCurrentDirectory here
       removeFileMayNotExist ".git/index.tmp"
       readTree t "index.tmp"
       checkoutIndex "index.tmp" (toFilePath tdir++"/")
       removeFileMayNotExist ".git/index.tmp"
       setCurrentDirectory tdir
       when (Nice `elem` opts) $ nice 19
       ecb <- runIfPresent "./.build"
       case ecb of
         ExitFailure _ ->
             do setCurrentDirectory here
                havet <- doesFileExist ".test"
                if havet && Build `notElem` opts
                    then return Unresolved
                    else return Fail
         ExitSuccess | Build `elem` opts -> return Pass
         ExitSuccess -> do ec <- runIfPresent "./.test"
                           setCurrentDirectory here
                           case ec of ExitFailure _ -> return Fail
                                      ExitSuccess -> return Pass
    where runIfPresent x = do e <- doesFileExist x
                              if e then system x
                                   else return ExitSuccess

slurpTree :: Hash Tree C(x) -> IO (Slurpy C(x))
slurpTree = slurpTreeHelper (fp2fn ".")

slurpTreeHelper :: FileName -> Hash Tree C(x) -> IO (Slurpy C(x))
slurpTreeHelper rootdir t =
    do xs <- catTree t
       unsafeInterleaveIO $
             (Slurpy rootdir . SlurpDir (Just t). slurpies_to_map)
             `fmap` mapM sl xs
    where sl (n, Subtree t') = unsafeInterleaveIO $ slurpTreeHelper n t'
          sl (n, File h) =
              do x <- unsafeInterleaveIO $ catBlob h
                 return $ Slurpy n $ SlurpFile NotExecutable (Just h) x
          sl (n, Executable h) =
              do x <- unsafeInterleaveIO $ catBlob h
                 return $ Slurpy n $ SlurpFile IsExecutable (Just h) x
          sl (n, Symlink h) =
              do x <- unsafeInterleaveIO $ catBlob h
                 return $ Slurpy n $ SlurpSymlink x

writeSlurpTree :: Slurpy C(x) -> IO (Hash Tree C(x))
writeSlurpTree (Slurpy _ (SlurpDir (Just t) _)) = return t
writeSlurpTree (Slurpy _ (SlurpDir Nothing ccc)) =
    do debugMessage "starting writeSlurpTree"
       tes <- mapM writeSubsl $ map_to_slurpies ccc
       mkTree tes
    where writeSubsl (Slurpy fn (SlurpFile IsExecutable (Just h) _)) =
              return (fn, Executable h)
          writeSubsl (Slurpy fn (SlurpFile IsExecutable Nothing c)) =
              do h <- hashObject (`B.hPutStr` c)
                 return (fn, Executable h)
          writeSubsl (Slurpy fn (SlurpFile NotExecutable (Just h) _)) =
              return (fn, File h)
          writeSubsl (Slurpy fn (SlurpFile NotExecutable Nothing c)) =
              do h <- hashObject (`B.hPutStr` c)
                 return (fn, File h)
          writeSubsl (Slurpy fn (SlurpSymlink x)) =
              do h <- hashObject (`B.hPutStr` x)
                 return (fn, Symlink h)
          writeSubsl (Slurpy fn (SlurpDir (Just h) _)) =
              return (fn, Subtree h)
          writeSubsl d@(Slurpy fn (SlurpDir Nothing _)) =
              do h <- writeSlurpTree d
                 return (fn, Subtree h)
writeSlurpTree x = writeSlurpTree (Slurpy (fp2fn ".")
                                    (SlurpDir Nothing $ slurpies_to_map [x]))

simplifyParents :: [Flag] -> [Sealed (Hash Commit)]
                -> [String] -> Hash Tree C(x)
                -> IO (Sealed (Hash Commit))
simplifyParents opts pars0 log0 rec0 =
    do dependon0 <- concat `fmap` mapM remoteHeads [for | RecordFor for <- opts]
       let dependon = cauterizeHeads $
                      filter (\x -> x `elem` pars0 || any (x `iao`) pars0)
                             dependon0
           pars = pars0 `notIn` dependon
       Sealed t0 <- mergeCommits (pars0++dependon)
       srec <- slurpTree rec0
       p <- (\s0 -> diff opts s0 srec) `fmap` slurpTree t0
       testedby <- testMessage opts
       let testit xs =
            do Sealed sold <- mergeCommits (dependon++xs) >>= mapSealM slurpTree
               s0 <- slurpTree t0
               case commute (diff opts sold s0 :> p) of
                 Nothing -> return Nothing
                 Just (p' :> _) ->
                   do s' <- apply_to_slurpy p' sold
                      t' <- writeSlurpTree s'
                      let -- FIXME join with Signed-off-by:
                          cleanup ("":"":r) = cleanup ("":r)
                          cleanup (a:b) = a : cleanup b
                          cleanup [] = []
                          message = (unlines $ cleanup $ log0++testedby)
                      com <- commitTreeNicely opts t' (dependon++xs) message
                      if Test `elem` opts
                        then do tr <- testC [Sealed com]
                                if tr then return (Just $ Sealed com)
                                      else return Nothing
                        else return $ Just $ Sealed com
       allok <- testit pars
       c <- case allok of
            Nothing -> fail "test fails!"
            Just call ->
              if CauterizeAllHeads `elem` opts
              then return call
              else
                do noneok <- testit []
                   case noneok of
                     Just cnone -> return cnone
                     Nothing -> bisect testit dependon [] (pars, call)
       -- the following test should be redundant, and once I'm
       -- confident with this code, I should replace it with the
       -- creation of this merge commit without testing.  It might be
       -- better to add a caching of tests by tree hash, so that we
       -- could leave this code here, but it wouldn't actually re-test
       -- the same tree with a different history.  In fact, perhaps I
       -- should separate the cache of tested trees from the cache of
       -- merge commits, since they really are orthogonal.
       merg <- testC (c:pars0)
       if merg then return c
               else fail "Unexpected test failure in simplifyParents!"
    where testC cs = fmap (const True) (testCommits opts "Merge" cs)
                     `catch` \_ -> return False
          bisect testit dependon bad (good,cgood) =
               case good `notIn` (bad++dependon) of
                 [] -> return cgood
                 [_] -> return cgood
                 bs -> do let (try,try2) =
                                 case (take (length bs `div` 2) bs)
                                          `notIn` (bad++dependon) of
                                   x -> case good `notIn` (x++bad++dependon) of
                                          [] -> ([],[]) -- skip to snail
                                          y -> (x,y)
                          oktry <- if null try then return Nothing
                                               else testit (bad++try)
                          oktry2 <- if null try2 then return Nothing
                                                 else testit (bad++try2)
                          case (oktry, oktry2) of
                            (Just ttry, Nothing) ->
                                 do debugMessage "bisect with half"
                                    bisect testit dependon bad (bad++try,ttry)
                            (Nothing, Just ttry2) ->
                                 do debugMessage "bisect with other half"
                                    bisect testit dependon bad (bad++try2,ttry2)
                            (Nothing, Nothing) ->
                                 do debugMessage ("resorting to snail... "++
                                                  show (length good)++"/"++
                                                  show (length bad))
                                    snail testit dependon bad (good,cgood)
                            (Just ttry, Just _) ->
                                 do debugMessage "weird situation..."
                                    bisect testit dependon bad (bad++try,ttry)
          snail testit dependon bad (g0:good0,cgood) =
              do let g = findprim g0
                     good = filter (/= g) (g0:good0)
                 ok <- testit (g:bad)
                 case ok of Just tgbad -> return tgbad
                            Nothing ->
                                bisect testit dependon (g:bad) (good,cgood)
              where findprim x = case filter (/=x) ([x] `notIn` bad) of
                                   [] -> x
                                   y:_ -> findprim y
          snail _ _ _ ([],cgood) =
               do putStrLn "Weird business in snail!!!"
                  return cgood

mergeCommits :: [Sealed (Hash Commit)] -> IO (Sealed (Hash Tree))
mergeCommits hs0 =
    do hs <- cauterizeHeads `fmap` expandTrivialMerges hs0
       mt <- readCached hs
       case mt of
         Just t -> return t
         Nothing -> do Sealed t <- mergeCommitsX hs
                       cacheTree hs t
                       return (Sealed t)

mergeCommitsX :: [Sealed (Hash Commit)] -> IO (Sealed (Hash Tree))
mergeCommitsX hs0 =
 case cauterizeHeads hs0 of
 [] -> Sealed `fmap` writeSlurpTree empty_slurpy
 [Sealed h] -> Sealed `fmap` catCommitTree h
 hs ->
    do let cp = take 1 $ chokePoints hs
           f0:fs0 = map (\h -> [h] `notIn` cp) hs
           primaryBase = cauterizeHeads [x | x <- f0, all (elem x) fs0]
           boring = filter tooOld $ nub $ concat (f0:fs0)
           tooOld x = not $ all (`iao` x) primaryBase
           families = map (filter (`notElem` boring)) (f0:fs0)
           uniques = concatMap singles $ group $ sort $ concat families
           singles [x] = [x]
           singles _ = []
           secondaryBase = cauterizeHeads
                           (concat families \\ (primaryBase++uniques))
       Sealed secondaryTree <- mergeCommits (primaryBase++secondaryBase++cp)
       let mergeOne (Sealed h) =
               do Sealed t <- mergeCommits (Sealed h:secondaryBase)
                  msg <- (concat.take 1.lines.myMessage) `fmap` catCommit h
                  old <- slurpTree secondaryTree
                  new <- slurpTree t
                  return $ Sealed $ mapFL_FL (infopatch msg) $ diff [] old new
       ps <- mapM mergeOne hs
       Sealed newp <- return $ mergeNamed ps
       old <- slurpTree secondaryTree
       merged <- apply_to_slurpy newp old
       Sealed `fmap` writeSlurpTree merged

cacheKey :: [Sealed (Hash Commit)] -> String
cacheKey hs = "new merge "++show (sort hs)

cacheTree :: [Sealed (Hash Commit)] -> Hash Tree C(x) -> IO ()
cacheTree x t =
    do k <- hashObject (`hPutStrLn` cacheKey x)
       c <- commitTree t (sort x) (cacheKey x)
       updateref ("refs/merges/"++show k) (Sealed c) Nothing
                     `catch` (\_ -> return ())
       return ()

readCached :: [Sealed (Hash Commit)] -> IO (Maybe (Sealed (Hash Tree)))
readCached x =
    do k <- hashObject (`hPutStrLn` cacheKey x)
       Just `fmap`
                (parseRev ("refs/merges/"++show k) >>= mapSealM catCommitTree)
    `catch` \_ -> return Nothing

diffCommit :: [Flag] -> Hash Commit C(x)
           -> IO (FlippedSeal (FL Prim) C(x))
diffCommit _ c0 =
    do c <- catCommit c0
       new <- slurpTree $ myTree c
       Sealed oldh <- mergeCommits (myParents c)
       old <- slurpTree oldh
       return $ FlippedSeal $ diff [] old new

revListHeads :: [RevListOption] -> IO String
revListHeads revlistopts =
    do hs <- cauterizeHeads `fmap` heads
       if null hs
          then return ""
          else revList (map show hs) revlistopts

revListHeadsHashes :: [RevListOption] -> IO [Sealed (Hash Commit)]
revListHeadsHashes revlistopts =
    do hs <- cauterizeHeads `fmap` heads
       if null hs
          then return []
          else revListHashes hs revlistopts

configDefaults :: Maybe String -> String
               -> [Flag -> [Either String (String,String)]] -> [Flag] -> IO ()
configDefaults msuper cmd cs fs = mapM_ configit xs
    where xs = concat [c f | f <- fs, c <- cs ]
          configit (Left x) = unsetConfig opts $ fname x
          configit (Right (a,b)) = setConfig opts (fname a) b
          fname x = case msuper of
                      Just super -> "iolaus."++super++'.':cmd++'.':x
                      Nothing -> "iolaus."++cmd++'.':x
          opts = if GlobalConfig `elem` fs
                 then [Global]
                 else if SystemConfig `elem` fs
                      then [System]
                      else []

-- The following "trivial merges" stuff is a workaround for a proper
-- algorithm for merging conflict resolutions, which at least keeps
-- "automerge" commits from causing trouble.  I'm not sure what a
-- proper merge algorithm will look like... :(

expandTrivialMerges :: [Sealed (Hash Commit)] -> IO [Sealed (Hash Commit)]
expandTrivialMerges [] = return []
expandTrivialMerges (c:cs) =
    do itm <- isTrivialMerge c
       rest <- expandTrivialMerges cs
       return $ if itm then unseal parents c ++ rest
                       else c : rest

isTrivialMerge :: Sealed (Hash Commit) -> IO Bool
isTrivialMerge (Sealed c) =
    do ce <- catCommit c
       if take 5 (myMessage ce) == "Merge"
          then do FlippedSeal ch <- diffCommit [] c
                  case ch of
                    NilFL -> return True
                    _ -> return False
          else return False

showCommit :: [Flag] -> Hash Commit C(x) -> IO Doc
showCommit opts c =
    do commit <- catCommit c
       subjauthordat <- formatRev "%s%n%aN <%ae>  %ad" c
       let x = if ShowHash `elem` opts then text (show c) $$
                                            text subjauthordat $$
                                            text (mypretty commit)
                                       else text subjauthordat $$
                                            text (mypretty commit)
       d <- if Summary `elem` opts
            then do FlippedSeal ch <- diffCommit opts c
                    return $ prefix "    " (summarize ch)
            else if Verbose `elem` opts
                 then do FlippedSeal ch <- diffCommit opts c
                         new <- slurpTree (myTree commit)
                         let Just old = apply_to_slurpy (invert ch) new
                         return $ showContextPatch old ch
                 else return empty
       let ps = if ShowParents `elem` opts
                then case map show $ parents c of
                       [] -> empty
                       pars -> text ("Parents: "++unwords pars)
                else empty
       return (x $$ ps $$ d)
    where
      mypretty ce = unlines (map ("    "++) cl)
          where _:cl0 = lines $ myMessage ce
                cl = if ShowTested `elem` opts
                     then cl0
                     else reverse $ clearout $ reverse cl0
                clearout (x:xs) | take 6 x == "Tested" = dropWhile (=="") xs
                                | take 5 x == "Built" = dropWhile (=="") xs
                                | otherwise = x:xs
                clearout [] = []
