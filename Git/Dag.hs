{-# LANGUAGE CPP, GADTs #-}
#include "gadts.h"

module Git.Dag ( parents, ancestors, allAncestors, isAncestorOf, notIn,
                 mergeBases, cauterizeHeads, dag2commit,
                 makeDag, Dag(..), greatGrandFather,
                 commonAncestors, uncommonAncestors,
                 Bisection(..), bisect, bisectionPlan ) where

import Data.Maybe ( catMaybes )
import Data.List ( sort, partition, nub )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import System.IO.Unsafe ( unsafePerformIO )

import Iolaus.Ordered ( EqCheck(..), unsafeCoerceP )
import Iolaus.Sealed ( Sealed(Sealed), unseal )
import Iolaus.Show ( eq1 )
import Git.Plumbing ( Hash, Commit, catCommit, CommitEntry(..) )

data CommitLinks = CommitLinks { cancestors :: S.Set (Sealed (Hash Commit)),
                                 cparents :: [Sealed (Hash Commit)] }
                 deriving ( Eq )
instance Ord CommitLinks where
    compare x y = compare (S.size $ cancestors x) (S.size $ cancestors y)

{-# NOINLINE genealogy #-}
genealogy :: IORef (M.Map (Sealed (Hash Commit)) CommitLinks)
genealogy = unsafePerformIO $ newIORef M.empty

isAncestorOf :: Hash Commit C(x) -> Hash Commit C(y) -> Bool
a `isAncestorOf` b = Sealed a `S.member` ancestors b

commonAncestors :: Hash Commit C(x) -> Hash Commit C(y)
                -> S.Set (Sealed (Hash Commit))
commonAncestors a b = S.intersection (ancestors a) (ancestors b)

uncommonAncestors :: [Sealed (Hash Commit)] -> S.Set (Sealed (Hash Commit))
uncommonAncestors xs = S.difference (S.unions axs) com
    where axs = map (unseal ancestors) xs
          com = int S.empty axs
          int i [] = i
          int i (s:ss) = int (S.intersection i s) ss

notIn ::  [Sealed (Hash Commit)] ->  [Sealed (Hash Commit)]
      -> [Sealed (Hash Commit)]
notIn them us = newestFirst $ S.toList justhem
    where allus = S.unions $ S.fromList us : map (unseal ancestors) us
          allthem = S.unions $ S.fromList them : map (unseal ancestors) them
          justhem = S.difference allthem allus

mergeBases :: [Sealed (Hash Commit)] -> [Sealed (Hash Commit)]
mergeBases [] = []
mergeBases (h:hs) = mb (unseal ancestors h) hs
    where mb a [] = fmb 0 [] $ S.toList a
          mb a (x:xs) = mb (S.intersection a $ unseal ancestors x) xs
          fmb _ sofar [] = sofar
          fmb na sofar (x:xs) = if nx > na
                                then fmb nx [x] xs
                                else if nx == na
                                     then fmb na (x:sofar) xs
                                     else fmb na sofar xs
              where nx = S.size $ unseal ancestors x

ancestors :: Hash Commit C(x) -> S.Set (Sealed (Hash Commit))
ancestors h = unsafePerformIO $ findAncestors $ Sealed h

allAncestors :: [Sealed (Hash Commit)] -> [Sealed (Hash Commit)]
allAncestors hs = nub (hs ++ concatMap (unseal parents) hs ++
                       newestFirst (S.toList s))
    where s = S.unions $ map (unseal ancestors) hs

parents :: Hash Commit C(x) -> [Sealed (Hash Commit)]
parents h = unsafePerformIO $
    do ms <- M.lookup (Sealed h) `fmap` readIORef genealogy
       case ms of
         Just s -> return $ cparents s
         Nothing -> do findAncestors (Sealed h) -- to compute and cache...
                       ms' <- M.lookup (Sealed h) `fmap` readIORef genealogy
                       case ms' of
                         Just s -> return $ cparents s
                         Nothing -> fail "aack in findParents"

findAncestors :: Sealed (Hash Commit) -> IO (S.Set (Sealed (Hash Commit)))
findAncestors h@(Sealed hh) =
    do ms <- M.lookup h `fmap` readIORef genealogy
       case ms of
         Just s -> return $ cancestors s
         Nothing -> do ps <- myParents `fmap` catCommit hh
                       as <- mapM findAncestors ps
                       let cl = CommitLinks (S.unions $ S.fromList ps:as) ps
                       modifyIORef genealogy $ M.insert h cl
                       return $ cancestors cl

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
cauterizeHeads0 (Sealed x:xs)
  | any (unseal (isAncestorOf x)) xs = cauterizeHeads0 xs
  | Sealed x `elem` xs = cauterizeHeads0 xs
  | otherwise = Sealed x :
                cauterizeHeads0 (filter (unseal (not . (`isAncestorOf` x))) xs)

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
