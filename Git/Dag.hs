{-# LANGUAGE CPP, GADTs #-}
#include "gadts.h"

module Git.Dag ( parents, ancestors, isAncestorOf,
                 mergeBases, cauterizeHeads,
                 makeDag, Dag(..), greatGrandFather,
                 commonAncestors, uncommonAncestors ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import System.IO.Unsafe ( unsafePerformIO )

import Iolaus.Ordered ( EqCheck(..), unsafeCoerceP )
import Iolaus.Sealed ( Sealed(Sealed), unseal, mapSeal )
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

findChildren :: Hash Commit C(x) -> S.Set (Sealed (Hash Commit))
             -> S.Set (Sealed (Hash Commit))
findChildren p = S.filter (elem (Sealed p) . unseal parents)

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

sameHash :: Hash a C(x) -> Hash a C(y) -> EqCheck C(x y)
sameHash a b = if eq1 a b then unsafeCoerceP IsEq else NotEq

makeDag :: Hash Commit C(x) -> Hash Commit C(y) -> Dag C(x y)
makeDag a me =
    case sameHash a me of
      IsEq -> Ancestor me
      NotEq -> Node me $ map (mapSeal (makeDag a)) $ parents me

greatGrandFather :: Dag C(x y) -> Hash Commit C(x)
greatGrandFather (Ancestor a) = a
greatGrandFather (Node _ []) = error "guy with no ancestors?"
greatGrandFather (Node _ (Sealed x:_)) = greatGrandFather x

cauterizeHeads :: [Sealed (Hash Commit)] -> [Sealed (Hash Commit)]
cauterizeHeads [] = []
cauterizeHeads [a] = [a]
cauterizeHeads (Sealed x:xs)
    | any (unseal (`isAncestorOf` x)) xs = cauterizeHeads xs
    | otherwise = Sealed x :
                  cauterizeHeads (filter (unseal (not . isAncestorOf x)) xs)
