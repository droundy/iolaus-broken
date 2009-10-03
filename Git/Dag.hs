module Git.Dag ( parents, ancestors, isAncestorOf,
                 mergeBases ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import System.IO.Unsafe ( unsafePerformIO )

import Git.Plumbing ( Hash, Commit, catCommit, CommitEntry(..) )

data CommitLinks = CommitLinks { cancestors :: S.Set (Hash Commit),
                                 cparents :: [Hash Commit] }
                 deriving ( Eq )
instance Ord CommitLinks where
    compare x y = compare (S.size $ cancestors x) (S.size $ cancestors y)

{-# NOINLINE genealogy #-}
genealogy :: IORef (M.Map (Hash Commit) CommitLinks)
genealogy = unsafePerformIO $ newIORef M.empty

isAncestorOf :: Hash Commit -> Hash Commit -> Bool
a `isAncestorOf` b = a `S.member` ancestors b

commonAncestors :: Hash Commit -> Hash Commit -> S.Set (Hash Commit)
commonAncestors a b = S.intersection (ancestors a) (ancestors b)

mergeBases :: [Hash Commit] -> [Hash Commit]
mergeBases [] = []
mergeBases (h:hs) = mb (ancestors h) hs
    where mb a [] = fmb 0 [] $ S.toList a
          mb a (x:xs) = mb (S.intersection a $ ancestors x) xs
          fmb _ sofar [] = sofar
          fmb na sofar (x:xs) = if nx > na
                                then fmb nx [x] xs
                                else if nx == na
                                     then fmb na (x:sofar) xs
                                     else fmb na sofar xs
              where nx = S.size $ ancestors x

ancestors :: Hash Commit -> S.Set (Hash Commit)
ancestors h = unsafePerformIO $ findAncestors h

parents :: Hash Commit -> [Hash Commit]
parents h = unsafePerformIO $
    do ms <- M.lookup h `fmap` readIORef genealogy
       case ms of
         Just s -> return $ cparents s
         Nothing -> do findAncestors h -- to compute and cache...
                       ms' <- M.lookup h `fmap` readIORef genealogy
                       case ms' of
                         Just s -> return $ cparents s
                         Nothing -> fail "aack in findParents"

findAncestors :: Hash Commit -> IO (S.Set (Hash Commit))
findAncestors h =
    do ms <- M.lookup h `fmap` readIORef genealogy
       case ms of
         Just s -> return $ cancestors s
         Nothing -> do ps <- myParents `fmap` catCommit h
                       as <- mapM findAncestors ps
                       let cl = CommitLinks (S.unions $ S.fromList ps:as) ps
                       modifyIORef genealogy $ M.insert h cl
                       return $ cancestors cl
