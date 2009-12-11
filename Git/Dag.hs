{-# LANGUAGE CPP, GADTs #-}
#include "gadts.h"

module Git.Dag ( parents, isAncestorOf, iao, notIn,
                 chokePoints, cauterizeHeads, dag2commit,
                 makeDag, Dag(..), greatGrandFather,
                 Bisection(..), bisect, bisectionPlan ) where

import Data.Maybe ( catMaybes )
import Data.List ( sort, partition )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import System.IO.Unsafe ( unsafePerformIO )

import Iolaus.Ordered ( EqCheck(..), unsafeCoerceP )
import Iolaus.Sealed ( Sealed(Sealed), unseal )
import Iolaus.Show ( eq1 )
import Iolaus.Global ( debugMessage )
import Git.Plumbing ( Hash, Commit, catCommit, CommitEntry(..) )

data CommitLinks =
    CommitLinks { cancestors :: Maybe (S.Set (Sealed (Hash Commit))),
                  cparents :: [Sealed (Hash Commit)] }
    deriving ( Eq )

{-# NOINLINE genealogy #-}
genealogy :: IORef (M.Map (Sealed (Hash Commit)) CommitLinks)
genealogy = unsafePerformIO $ newIORef M.empty

isAncestorOf :: Hash Commit C(x) -> Hash Commit C(y) -> Bool
a `isAncestorOf` b
    | Sealed a == Sealed b = False
    | otherwise =
        case chokePoints [Sealed a,Sealed b] of
          c:_ -> Sealed a == c || Sealed a `elem` lazyAncestorsTo [Sealed b] c
          [] -> Sealed a `S.member` ancestors b

iao :: Sealed (Hash Commit) -> Sealed (Hash Commit) -> Bool
Sealed a `iao` Sealed b = a `isAncestorOf` b

notIn ::  [Sealed (Hash Commit)] ->  [Sealed (Hash Commit)]
      -> [Sealed (Hash Commit)]
-- "us as an empty list is a special case, because the result of
-- chokePoints (them++us) is not present in us.
notIn them [] = S.toList (S.unions $ S.fromList them :
                          map (unseal ancestors) them)
notIn them us = newestFirst $ S.toList justhem
    where (allus,allthem) =
              case chokePoints (them++us) of
                c:_ -> (S.fromList $ filter (/=c) us++lazyAncestorsTo us c,
                        S.fromList $ filter (/=c) them++lazyAncestorsTo them c)
                [] ->(S.unions $ S.fromList us : map (unseal ancestors) us,
                      S.unions $ S.fromList them : map (unseal ancestors) them)
          justhem = S.difference allthem allus

{-# NOINLINE chokes #-}
chokes :: IORef (M.Map (Sealed (Hash Commit)) (Maybe (Sealed (Hash Commit))))
chokes = unsafePerformIO $ newIORef M.empty

chokePoints :: [Sealed (Hash Commit)] -> [Sealed (Hash Commit)]
chokePoints [] = []
chokePoints [h0] = unsafePerformIO $
                   do x <- M.lookup h0 `fmap` readIORef chokes
                      case x of
                        Just (Just c) -> return $ h0 : chokePoints [c]
                        Just Nothing -> return [h0]
                        Nothing -> do let cs = chokePoints (unseal parents h0)
                                          c = case cs of [] -> Nothing
                                                         y:_ -> Just y
                                      modifyIORef chokes $ M.insert h0 c
                                      return (h0:cs)
chokePoints hs0 = join_same $ zip (repeat S.empty)
                                  (map (chokePoints . (:[])) hs0)
    where join_same :: [(S.Set (Sealed (Hash Commit)), [Sealed (Hash Commit)])]
                    -> [Sealed (Hash Commit)]
          join_same [] = []
          join_same [(_,x)] = x
          join_same cs | [] `elem` map snd cs =
                           case partition (null . snd) cs of
                             (es,os) ->
                                 join_same $ map fixit os
                                     where fixit (a,b) = (a,filter isok b)
                                           isok x = all (S.member x)$ map fst es
          join_same cs =
              case filter (\h -> all (h `S.member`) passedby) hs of
                h:_ -> case filter ((h==) . head) $ map snd cs of
                         rest':_ -> rest'
                         [] -> error "bug in chokePoints"
                [] -> join_same $ zip passedby $ map (tail . snd) cs
              where hs = map (head . snd) cs
                    passedby = zipWith S.insert hs (map fst cs)

ancestors :: Hash Commit C(x) -> S.Set (Sealed (Hash Commit))
ancestors h = unsafePerformIO $ findAncestors $ Sealed h

lazyAncestorsTo :: [Sealed (Hash Commit)] -> Sealed (Hash Commit)
                -> [Sealed (Hash Commit)]
lazyAncestorsTo hs0 downto = la (S.singleton downto) $ filter (/= downto) hs0
    where la _ [] = []
          la sofar (h:hs) =
              case filter (not . (`S.member` sofar)) $ unseal parents h of
                [] -> la (S.insert h sofar) hs
                ps -> ps ++ la (S.union (S.fromList ps) sofar) (hs++ps)

parents :: Hash Commit C(x) -> [Sealed (Hash Commit)]
parents h = unsafePerformIO $
    do ms <- M.lookup (Sealed h) `fmap` readIORef genealogy
       case ms of
         Just s -> return $ cparents s
         Nothing -> do ms' <- M.lookup (Sealed h) `fmap` readIORef genealogy
                       case ms' of
                         Just s -> return $ cparents s
                         Nothing ->
                             do debugMessage ("parents "++show h)
                                ps <- myParents `fmap` catCommit h
                                modifyIORef genealogy $ M.insert (Sealed h) $
                                  CommitLinks { cancestors = Nothing,
                                                cparents = ps}
                                return ps

findAncestors :: Sealed (Hash Commit) -> IO (S.Set (Sealed (Hash Commit)))
findAncestors h =
    do ms <- M.lookup h `fmap` readIORef genealogy
       case ms >>= cancestors of
         Just xx -> return xx
         Nothing ->
             do debugMessage ("more findAncestors "++show h)
                let ps = unseal parents h
                as <- mapM findAncestors ps
                let a = S.unions $ S.fromList ps:as
                modifyIORef genealogy $ M.insert h $
                            CommitLinks { cancestors = Just a,
                                          cparents = ps }
                return a

data Dag C(x y) where
    Node :: Hash Commit C(y) -> [Sealed (Dag C(x))] -> Dag C(x y)
    Ancestor :: Hash Commit C(x) -> Dag C(x x)

dag2commit :: Dag C(x y) -> Hash Commit C(y)
dag2commit (Node x _) = x
dag2commit (Ancestor x) = x

sameHash :: Hash a C(x) -> Hash a C(y) -> EqCheck C(x y)
sameHash a b = if eq1 a b then unsafeCoerceP IsEq else NotEq

makeDag :: Hash Commit C(x) -> Hash Commit C(y) -> Maybe (Dag C(x y))
makeDag a me =
    case sameHash a me of
      IsEq -> Just $ Ancestor me
      NotEq -> do let mkdag (Sealed h) = do d <- makeDag a h
                                            Just (Sealed d)
                  pdags@(_:_) <- Just $ catMaybes $ map mkdag $ parents me
                  Just $ Node me pdags


greatGrandFather :: Dag C(x y) -> Hash Commit C(x)
greatGrandFather (Ancestor a) = a
greatGrandFather (Node h []) = error (show h++" has no ancestors?")
greatGrandFather (Node _ (Sealed x:_)) = greatGrandFather x

cauterizeHeads :: [Sealed (Hash Commit)] -> [Sealed (Hash Commit)]
cauterizeHeads = newestFirst . cauterizeHeads0

cauterizeHeads0 :: [Sealed (Hash Commit)] -> [Sealed (Hash Commit)]
cauterizeHeads0 [] = []
cauterizeHeads0 [a] = [a]
cauterizeHeads0 (x:xs)
  | x `elem` xs = cauterizeHeads0 xs
  | any (x `iao`) xs = cauterizeHeads0 xs
  | otherwise = x : cauterizeHeads0 (filter (not . (`iao` x)) xs)

data Bisection a = Test a (Bisection a) (Bisection a) | Done a

instance Functor Bisection where
    f `fmap` Done a = Done (f a)
    f `fmap` Test a y n = Test (f a) (f `fmap` y) (f `fmap` n)

bisect :: Monad m => (a -> m Bool) -> Bisection a -> m a
bisect _ (Done a) = return a
bisect test (Test a iftrue iffalse) =
    do x <- test a
       if x then bisect test iftrue
            else bisect test iffalse

bisectionPlan :: Hash Commit C(x) -> [Sealed (Hash Commit)]
              -> Bisection (Sealed (Hash Commit))
bisectionPlan ancestor heads =
    easyBisection $ S.toList $
                  S.difference (S.unions $ map (unseal ancestors) heads)
                               (ancestors ancestor)

newestFirst :: [Sealed (Hash Commit)] -> [Sealed (Hash Commit)]
newestFirst cs = map snd $ sort (zip ages cs)
    where ages = map (\c -> length $ filter (ia c) cs) cs
          ia (Sealed x) (Sealed y) = isAncestorOf x y

easyBisection :: [Sealed (Hash Commit)] -> Bisection (Sealed (Hash Commit))
easyBisection [] = error "oops in easyBisection"
easyBisection [c] = Done c
easyBisection cs =
    case drop (length cs `div` 2) $ newestFirst cs of
      pivot:_ -> case partition (ia pivot) cs of
                   (a,b) -> Test pivot (easyBisection a) (easyBisection b)
      [] -> error "nothing in easyBisection"
    where ia (Sealed x) (Sealed y) = isAncestorOf x y
