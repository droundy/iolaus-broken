{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}

--  Copyright (C) 2002,2008-2009 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software Foundation,
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

module Grit.Lcs2 ( getChanges, patientChanges, smartChanges,
                         patientLcs ) where

import Data.List ( sort )
import Data.Array.ST
import Control.Monad.ST
import qualified Data.Set as S

import qualified Data.ByteString as B (ByteString)

#include "impossible.h"

{-# SPECIALIZE smartChanges :: [B.ByteString] -> [B.ByteString]
                              -> [(Int, [B.ByteString], [B.ByteString])] #-}
smartChanges :: Ord a => [a] -> [a] -> [(Int,[a],[a])]
smartChanges o n | length o > 10000 = getChanges o n
                 | otherwise = patientChanges o n

{-# SPECIALIZE patientChanges :: [B.ByteString] -> [B.ByteString]
                              -> [(Int, [B.ByteString], [B.ByteString])] #-}
patientChanges :: Ord a => [a] -> [a] -> [(Int,[a],[a])]
patientChanges o n = mkdiff 0 (patientLcs o n) o n

{-# SPECIALIZE getChanges :: [B.ByteString] -> [B.ByteString]
                          -> [(Int, [B.ByteString], [B.ByteString])] #-}
getChanges :: Ord a => [a] -> [a] -> [(Int,[a],[a])]
getChanges o n = mkdiff 0 (lcs o n) o n

mkdiff :: Ord a => Int -> [a] -> [a] -> [a] -> [(Int,[a],[a])]
mkdiff ny (l:ls) (x:xs) (y:ys) | l == x && l == y = mkdiff (ny+1) ls xs ys
mkdiff ny (l:ls) xs ys = do let rmd = takeWhile (/= l) xs
                                add = takeWhile (/= l) ys
                                restx = drop (length rmd + 1) xs
                                resty = drop (length add + 1) ys
                            (ny, rmd, add) : mkdiff (ny+length add+1) ls restx resty
mkdiff _ [] [] [] = []
mkdiff ny [] xs ys = [(ny, xs, ys)]

-- | The patientLcs algorithm is inspired by the "patience" algorithm
-- (for which I don't have a reference handy), in that it looks for
-- unique lines, and uses them to subdivide the problem.  I use lcs to
-- diff the unique lines.  It is slower, but should lead to "better"
-- diffs, in the sense of ones that better align with what humans
-- think changed.
-- 
-- Note that when compared with the Meyers algorithm used in darcs,
-- this is somewhat slower (maybe 4x in some of my tests, but is
-- lacking a stack overflow bug.  I'm not sure how it scales in
-- general, but it scales fine (just 10x slower than GNU diff) when
-- comparing a 6M american english dictionary with a british english
-- dictionary of the same size (which isn't a great test, but is the
-- largest pair of somewhat-differing files I could find).
-- 
-- Note that the patientLcs algorithm is slower than the one used in
-- lcs for sequences with mostly unique elements (as is common in text
-- files), but much *faster* when the sequence has a high degree of
-- redundancy.  i.e. lines /usr/share/dict/words vs lines (cat
-- /usr/share/dict/words | tr 'a-z' 'a')

{-# SPECIALIZE patientLcs ::[B.ByteString] -> [B.ByteString] -> [B.ByteString] #-}
patientLcs :: Ord a => [a] -> [a] -> [a]
patientLcs [] _ = []
patientLcs _ [] = []
patientLcs (c1:c1s) (c2:c2s)
    | c1 == c2 = c1: patientLcs c1s c2s
    | otherwise =
        reverse $ patientLcs0 (reverse (c1:c1s)) (reverse (c2:c2s))

patientLcs0 :: Ord a => [a] -> [a] -> [a]
patientLcs0 xs0@(cc1:cc1s) ys0@(cc2:cc2s)
    | cc1 == cc2 = cc1 : patientLcs0 cc1s cc2s
    | otherwise = case (filter (`S.member`uys) xs0, filter (`S.member`uxs) ys0) of
                    ([],_) -> lcs xs0 ys0
                    (_,[]) -> lcs xs0 ys0
                    (xs',ys') -> joinU (lcs xs' ys') xs0 ys0
    where uxs = findUnique xs0
          uys = findUnique ys0
          joinU [] x y = lcs x y
          joinU (b:bs) cs ds =
                 case break (==b) cs of
                   ([],_:c2) -> b : joinU bs c2 (drop 1 $ dropWhile (/= b) ds)
                   (c1,_:c2) -> case break (==b) ds of
                                  ([],_:d2) -> b : joinU bs c2 d2
                                  (d1,_:d2) -> lcs c1 d1 ++ b : joinU bs c2 d2
                                  _ -> impossible
                   _ -> impossible
          findUnique xs = S.fromList $ gru $ sort xs
          gru (x:x':xs) | x == x' = gru (dropWhile (==x) xs)
          gru (x:xs) = x : gru xs
          gru [] = []
          --findUnique xs = fu S.empty S.empty xs
          --    where fu _ uni [] = uni
          --          fu multi uni (y:ys)
          --              | y `S.member` multi = fu multi uni ys
          --              | y `S.member` uni = fu (S.insert y multi) (S.delete y uni) ys
          --              | otherwise = fu multi (S.insert y uni) ys
patientLcs0 [] _ = []
patientLcs0 _ [] = []

-- | ``LCS'' stands for ``Longest Common Subsequence,'' and it is a relatively
-- challenging problem to find an LCS efficiently.  I'm not going to explain
-- here what an LCS is, but will point out that it is useful in finding how
-- two sequences (lists, in this case) differ.  This module implements the
-- Hunt-Szymanski algorithm, which is appropriate for applications in which
-- the sequence is on an infinite alphabet, such as diffing the lines in two
-- files, where many, or most lines are unique.  In the best case scenario, a
-- permutation of unique lines, this algorithm is $O(n\log n)$.  In the worst
-- case scenario, that of a finite alphabet (i.e.\ where the number of elements
-- in the sequence is much greater than the number of unique elements), it is
-- an $O(n^2\log n)$ algorithm, which is pretty terrible.

{-# SPECIALIZE lcs ::[B.ByteString] -> [B.ByteString] -> [B.ByteString] #-}
lcs :: Ord a => [a] -> [a] -> [a]
lcs [] _ = []
lcs _ [] = []
lcs (c1:c1s) (c2:c2s)
    | c1 == c2 = c1: lcs c1s c2s
    | otherwise =
        reverse $ lcs_simple (reverse (c1:c1s)) (reverse (c2:c2s))

lcs_simple :: Ord a => [a] -> [a] -> [a]
lcs_simple [] _ = []
lcs_simple _ [] = []
lcs_simple s1@(c1:c1s) s2@(c2:c2s)
    | c1 == c2 = c1: lcs c1s c2s
    | otherwise = hunt $ prune_matches s1 $! find_matches s1 s2

prune_matches :: [a] -> [[Int]] -> [(a, [Int])]
prune_matches _ [] = []
prune_matches [] _ = []
prune_matches (_:cs) ([]:ms) = prune_matches cs ms
prune_matches (c:cs) (m:ms) = (c,m): prune_matches cs ms

type Threshold s a = STArray s Int (Int,[a])

hunt :: [(a, [Int])] -> [a]
hunt [] = []
hunt csmatches =
    runST ( do th <- empty_threshold (length csmatches) l
               hunt_internal csmatches th
               hunt_recover th (-1) l )
    where l = maximum (0 : concat (map snd csmatches))

hunt_internal :: [(a, [Int])] -> Threshold s a -> ST s ()
hunt_internal [] _ = return ()
hunt_internal ((c,m):csms) th = do
    hunt_one_char c m th
    hunt_internal csms th

hunt_one_char :: a -> [Int] ->  Threshold s a -> ST s ()
hunt_one_char _ [] _ = return ()
hunt_one_char c (j:js) th = do
    index_k <- my_bs j th
    case index_k of
      Nothing -> return ()
      Just k -> do
        (_, rest) <- readArray th (k-1)
        writeArray th k (j, c:rest)
    hunt_one_char c js th

-- This is O(n), which is stupid.
hunt_recover :: Threshold s a -> Int -> Int -> ST s [a]
hunt_recover th n limit =
 do (_, th_max) <- getBounds th
    if n < 0
       then hunt_recover th th_max limit
       else if n == 0
            then return []
            else if n > th_max
                 then return []
                 else do (thn, sn) <- readArray th n
                         if thn <= limit
                             then return $ reverse sn
                             else hunt_recover th (n-1) limit

empty_threshold :: Int -> Int -> ST s (Threshold s a)
empty_threshold l th_max = do
  th <- newArray (0,l) (th_max+1, [])
  writeArray th 0 (0, [])
  return th

my_bs :: Int -> Threshold s a -> ST s (Maybe Int)
my_bs j th = do bnds <- getBounds th
                my_helper_bs j bnds th

my_helper_bs :: Int -> (Int,Int) -> Threshold s a ->
                ST s (Maybe Int)
my_helper_bs j (th_min,th_max) th =
    if th_max - th_min > 1 then do
       (midth, _) <- readArray th th_middle
       if j > midth
         then my_helper_bs j (th_middle,th_max) th
         else my_helper_bs j (th_min,th_middle) th
    else do
       (minth, _) <- readArray th th_min
       (maxth, _) <- readArray th th_max
       if minth < j && maxth > j
          then return $ Just th_max
          else if j < minth then return $ Just th_min
               else return Nothing
    where th_middle = (th_max+th_min) `div` 2



find_matches :: Ord a => [a] -> [a] -> [[Int]]
find_matches [] [] = []
find_matches [] (_:bs) = []: find_matches [] bs
find_matches _ [] = []
find_matches a b =
    unzip_indexed $ sort $ find_sorted_matches indexeda indexedb [] []
    where indexeda = sort $ zip a [1..]
          indexedb = sort $ zip b [1..]

unzip_indexed :: [(Int,[a])] -> [[a]]
unzip_indexed s = unzip_indexed_helper 1 s
    where unzip_indexed_helper _ [] = []
          unzip_indexed_helper thisl ((l,c):rest)
           | thisl == l = c: unzip_indexed_helper (l+1) rest
           | otherwise = []: unzip_indexed_helper (thisl+1) ((l,c):rest)

find_sorted_matches :: Ord a => [(a, Int)] -> [(a, Int)] -> [a] -> [Int]
                             -> [(Int, [Int])]
find_sorted_matches [] _ _ _ = []
find_sorted_matches _ [] _ _ = []
find_sorted_matches ((a,na):as) ((b,nb):bs) aold aoldmatches
    | [a] == aold = (na, aoldmatches) :
                  find_sorted_matches as ((b,nb):bs) aold aoldmatches
    | a > b = find_sorted_matches ((a,na):as) bs aold aoldmatches
    | a < b = find_sorted_matches as ((b,nb):bs) aold aoldmatches
-- following line is inefficient if a line is repeated many times.
    | otherwise -- a == b
      = case reverse $ find_matches_one a ((b,nb):bs) of
        matches -> (na, matches) :
                   find_sorted_matches as ((b,nb):bs) [a] matches

find_matches_one :: Eq a => a -> [(a, Int)] -> [Int]
find_matches_one _ [] = []
find_matches_one a ((b,nb):bs)
    | a == b = nb: find_matches_one a bs
    | otherwise = []
