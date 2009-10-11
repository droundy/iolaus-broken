-- Copyright (C) 2002-2003 David Roundy
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

{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
-- , TypeOperators, PatternGuards #-}

#include "gadts.h"

module Iolaus.Patch.Permutations ( removeFL, removeRL, removeCommon,
                                  commuteWhatWeCanFL, commuteWhatWeCanRL,
                                  genCommuteWhatWeCanRL,
                                  partitionFL, partitionRL,
                                  head_permutationsFL, head_permutationsRL,
                                  headPermutationsFL,
                                  remove_subsequenceFL, remove_subsequenceRL ) where

import Data.Maybe ( catMaybes )
import Iolaus.Patch.Patchy ( Commute, commute, commuteFL, commuteRL, Invert(..), invertFL, invertRL )
import Iolaus.Ordered
#include "impossible.h"

-- |split an 'FL' into "left" and "right" lists according to a predicate, using commutation as necessary.
-- If a patch does satisfy the predicate but cannot be commuted past one that does not satisfy
-- the predicate, it goes in the "right" list.
partitionFL :: Commute p
            => (FORALL(u v) p C(u v) -> Bool)       -- ^predicate; if true we would like the patch in the "left" list
            -> FL p C(x y)                          -- ^input 'FL'
            -> (FL p :> FL p) C(x y)                -- ^"left" and "right" results

-- optimise by using an accumulating parameter to track all the "right" patches that we've found so far
partitionFL' :: Commute p
             => (FORALL(u v) p C(u v) -> Bool)
             -> RL p C(x z)  -- the "right" patches found so far
             -> FL p C(z y)
             -> (FL p :> FL p) C(x y)

partitionFL keepleft ps = partitionFL' keepleft NilRL ps

partitionFL' _ qs NilFL = NilFL :> reverseRL qs
partitionFL' keepleft qs (p :>: ps)
   | keepleft p,
     Just (p' :> qs') <- commuteRL (qs :> p)
       = case partitionFL' keepleft qs' ps of
         a :> b -> p' :>: a :> b
   | otherwise = partitionFL' keepleft (p :<: qs) ps

-- |split an 'RL' into "left" and "right" lists according to a predicate, using commutation as necessary.
-- If a patch does satisfy the predicate but cannot be commuted past one that does not satisfy
-- the predicate, it goes in the "left" list.
partitionRL :: Commute p
            => (FORALL(u v) p C(u v) -> Bool)    -- ^predicate; if true we would like the patch in the "right" list
            -> RL p C(x y)                       -- ^input 'RL'
            -> (RL p :> RL p) C(x y)             -- ^"left" and "right" results

-- optimise by using an accumulating parameter to track all the "left" patches that we've found so far
partitionRL' :: Commute p
             => (FORALL(u v) p C(u v) -> Bool)
             -> RL p C(x z)
             -> FL p C(z y)   -- the "left" patches found so far
             -> (RL p :> RL p) C(x y)

partitionRL keepright ps = partitionRL' keepright ps NilFL

partitionRL' _ NilRL qs = reverseFL qs :> NilRL

partitionRL' keepright (p :<: ps) qs
   | keepright p,
     Just (qs' :> p') <- commuteFL (p :> qs)
       = case partitionRL' keepright ps qs' of
         a :> b -> a :> p' :<: b
   | otherwise = partitionRL' keepright ps (p :>: qs)

commuteWhatWeCanFL :: Commute p => (p :> FL p) C(x y) -> (FL p :> p :> FL p) C(x y)
commuteWhatWeCanFL (p :> x :>: xs) =
    case commute (p :> x) of
    Nothing -> case commuteWhatWeCanFL (x :> xs) of
               xs1 :> x' :> xs2 -> case commuteWhatWeCanFL (p :> xs1) of
                              xs1' :> p' :> xs2' -> xs1' :> p' :> xs2' +>+ x' :>: xs2
    Just (x' :> p') -> case commuteWhatWeCanFL (p' :> xs) of
                       a :> p'' :> c -> x' :>: a :> p'' :> c
commuteWhatWeCanFL (y :> NilFL) = NilFL :> y :> NilFL

commuteWhatWeCanRL :: Commute p => (RL p :> p) C(x y) -> (RL p :> p :> RL p) C(x y)
commuteWhatWeCanRL = genCommuteWhatWeCanRL commute

genCommuteWhatWeCanRL :: (FORALL(a b) ((p :> p) C(a b) -> Maybe ((p :> p) C(a b))))
                      -> (RL p :> p) C(x y) -> (RL p :> p :> RL p) C(x y)
genCommuteWhatWeCanRL com (x :<: xs :> p) =
    case com (x :> p) of
    Nothing -> case genCommuteWhatWeCanRL com (xs :> x) of
               xs1 :> x' :> xs2 -> case genCommuteWhatWeCanRL com (xs2 :> p) of
                              xs1' :> p' :> xs2' -> xs1' +<+ x' :<: xs1 :> p' :> xs2'
    Just (p' :> x') -> case genCommuteWhatWeCanRL com (xs :> p') of
                       a :> p'' :> c -> a :> p'' :> x' :<: c
genCommuteWhatWeCanRL _ (NilRL :> y) = NilRL :> y :> NilRL


removeCommon :: (MyEq p, Commute p) => (FL p :\/: FL p) C(x y) -> (FL p :\/: FL p) C(x y)
removeCommon (xs :\/: NilFL) = xs :\/: NilFL
removeCommon (NilFL :\/: xs) = NilFL :\/: xs
removeCommon (xs :\/: ys) = rc xs (headPermutationsFL ys)
    where rc :: (MyEq p, Commute p) => FL p C(x y) -> [(p:>FL p) C(x z)] -> (FL p :\/: FL p) C(y z)
          rc nms ((n:>ns):_) | Just ms <- removeFL n nms = removeCommon (ms :\/: ns)
          rc ms [n:>ns] = ms :\/: n:>:ns
          rc ms (_:nss) = rc ms nss
          rc _ [] = impossible -- because we already checked for NilFL case

removeFL :: (MyEq p, Commute p) => p C(x y) -> FL p C(x z) -> Maybe (FL p C(y z))
removeFL x xs = r x $ headPermutationsFL xs
    where r :: (MyEq p, Commute p) => p C(x y) -> [(p:>FL p) C(x z)] -> Maybe (FL p C(y z))
          r _ [] = Nothing
          r z ((z':>zs):zss) | IsEq <- z =\/= z' = Just zs
                             | otherwise = r z zss

removeRL :: (MyEq p, Commute p) => p C(y z) -> RL p C(x z) -> Maybe (RL p C(x y))
removeRL x xs = r x $ head_permutationsRL xs
    where r :: (MyEq p, Commute p) => p C(y z) -> [RL p C(x z)] -> Maybe (RL p C(x y))
          r z ((z':<:zs):zss) | IsEq <- z =/\= z' = Just zs
                              | otherwise = r z zss
          r _ _ = Nothing

remove_subsequenceFL :: (MyEq p, Commute p) => FL p C(a b)
                     -> FL p C(a c) -> Maybe (FL p C(b c))
remove_subsequenceFL a b | lengthFL a > lengthFL b = Nothing
                         | otherwise = rsFL a b
    where rsFL :: (MyEq p, Commute p) => FL p C(a b) -> FL p C(a c) -> Maybe (FL p C(b c))
          rsFL NilFL ys = Just ys
          rsFL (x:>:xs) yys = removeFL x yys >>= remove_subsequenceFL xs

remove_subsequenceRL :: (MyEq p, Commute p) => RL p C(ab abc)
                     -> RL p C(a abc) -> Maybe (RL p C(a ab))
remove_subsequenceRL a b | lengthRL a > lengthRL b = Nothing
                         | otherwise = rsRL a b
    where rsRL :: (MyEq p, Commute p) => RL p C(ab abc) -> RL p C(a abc) -> Maybe (RL p C(a ab))
          rsRL NilRL ys = Just ys
          rsRL (x:<:xs) yys = removeRL x yys >>= remove_subsequenceRL xs

head_permutationsFL :: Commute p => FL p C(x y) -> [FL p C(x y)]
head_permutationsFL ps = map (\ (x:>xs) -> x:>:xs) $ headPermutationsFL ps

headPermutationsFL :: Commute p => FL p C(x y) -> [(p :> FL p) C(x y)]
headPermutationsFL NilFL = []
headPermutationsFL (p:>:ps) =
    (p:>ps) : catMaybes (map (swapfirstFL.(p:>)) $ headPermutationsFL ps)
        where swapfirstFL (p1:>p2:>xs) = do p2':>p1' <- commute (p1:>p2)
                                            Just $ p2':>p1':>:xs

head_permutationsRL :: Commute p => RL p C(x y) -> [RL p C(x y)]
head_permutationsRL NilRL = []
head_permutationsRL (p:<:ps) =
    (p:<:ps) : catMaybes (map (swapfirstRL.(p:<:)) $ head_permutationsRL ps)
        where swapfirstRL (p1:<:p2:<:xs) = do p1':>p2' <- commute (p2:>p1)
                                              Just $ p2':<:p1':<:xs
              swapfirstRL _ = Nothing

instance (MyEq p, Commute p) => MyEq (FL p) where
    a =\/= b | lengthFL a /= lengthFL b = NotEq
             | otherwise = cmpSameLength a b
             where cmpSameLength :: FL p C(x y) -> FL p C(x z) -> EqCheck C(y z)
                   cmpSameLength (x:>:xs) xys | Just ys <- removeFL x xys = cmpSameLength xs ys
                   cmpSameLength NilFL NilFL = IsEq
                   cmpSameLength _ _ = NotEq
    xs =/\= ys = reverseFL xs =/\= reverseFL ys

instance (Invert p, Commute p) => Invert (FL p) where
    invert = reverseRL . invertFL
    identity = NilFL
    sloppyIdentity NilFL = IsEq
    sloppyIdentity (x:>:xs) | IsEq <- sloppyIdentity x = sloppyIdentity xs
    sloppyIdentity _ = NotEq

instance (MyEq p, Commute p) => MyEq (RL p) where
    unsafeCompare = bug "Buggy use of unsafeCompare on RL"
    a =/\= b | lengthRL a /= lengthRL b = NotEq
             | otherwise = cmpSameLength a b
             where cmpSameLength :: RL p C(x y) -> RL p C(w y) -> EqCheck C(x w)
                   cmpSameLength (x:<:xs) xys | Just ys <- removeRL x xys = cmpSameLength xs ys
                   cmpSameLength NilRL NilRL = IsEq
                   cmpSameLength _ _ = NotEq
    xs =\/= ys = reverseRL xs =\/= reverseRL ys

instance (Commute p, Invert p) => Invert (RL p) where
    invert = reverseFL . invertRL
    identity = NilRL
    sloppyIdentity NilRL = IsEq
    sloppyIdentity (x:<:xs) | IsEq <- sloppyIdentity x = sloppyIdentity xs
    sloppyIdentity _ = NotEq
