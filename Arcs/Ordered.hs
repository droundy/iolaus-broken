-- Copyright (C) 2007 David Roundy
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

{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{-# LANGUAGE CPP #-}
-- , MagicHash, TypeOperators, GADTs #-}

#include "gadts.h"

module Arcs.Ordered ( EqCheck(..), isEq, (:>)(..), (:\/:)(..), (:/\:)(..),
                       FL(..), RL(..),Proof(..), (:>>)(..),
#ifndef GADT_WITNESSES
                             unsafeUnFL, unsafeFL, unsafeRL, unsafeUnRL,
#endif
                             lengthFL, mapFL, mapFL_FL, spanFL, foldlFL, allFL,
                             splitAtFL, bunchFL, foldlRL,
                             lengthRL, isShorterThanRL, mapRL, mapRL_RL, zipWithFL,
                             filterE, filterFL,
                             reverseFL, reverseRL, (+>+), (+<+),
                             nullFL, concatFL, concatRL, concatReverseFL,
                             MyEq, unsafeCompare, (=\/=), (=/\=),
                             consRLSealed, nullRL,
                             unsafeCoerceS, unsafeCoerceP, unsafeCoerceP2
                           ) where

import GHC.Base (unsafeCoerce#)
import Arcs.Show
import Arcs.Sealed ( FlippedSeal(..), flipSeal )

data EqCheck C(a b) where
    IsEq :: EqCheck C(a a)
    NotEq :: EqCheck C(a b)

instance Eq (EqCheck C(a b)) where
    IsEq == IsEq = True
    NotEq == NotEq = True
    _ == _ = False

isEq :: EqCheck C(a b) -> Bool
isEq IsEq = True
isEq NotEq = False

instance Show (EqCheck C(a b)) where
    show IsEq = "IsEq"
    show NotEq = "NotEq"

data Proof a C(x y) where
    Proof :: a -> Proof a C(x x)

data (a1 :>> a2) C(y) = FORALL(z) (a1 C(z)) :>> (a2 C(z y))
infixr 1 :>>

data (a1 :> a2) C(x y) = FORALL(z) (a1 C(x z)) :> (a2 C(z y))
infixr 1 :>
infix 1 :/\:, :\/:
data (a1 :\/: a2) C(x y) = FORALL(z) (a1 C(z x)) :\/: (a2 C(z y))
data (a1 :/\: a2) C(x y) = FORALL(z) (a1 C(x z)) :/\: (a2 C(y z))
class MyEq p where
    -- Minimal definition defines any one of unsafeCompare, =\/= and =/\=.
    unsafeCompare :: p C(a b) -> p C(c d) -> Bool
    unsafeCompare a b = IsEq == (a =/\= unsafeCoerceP b)
    (=\/=) :: p C(a b) -> p C(a c) -> EqCheck C(b c)
    a =\/= b | unsafeCompare a b = unsafeCoerceP IsEq
             | otherwise = NotEq
    (=/\=) :: p C(a c) -> p C(b c) -> EqCheck C(a b)
    a =/\= b | IsEq == (a =\/= unsafeCoerceP b) = unsafeCoerceP IsEq
             | otherwise = NotEq

infix 4 =\/=, =/\=

unsafeCoerceS :: a C(x) -> a C(b)
unsafeCoerceS = unsafeCoerce#

unsafeCoerceP :: a C(x y) -> a C(b c)
unsafeCoerceP = unsafeCoerce#

unsafeCoerceP2 :: t C(w x y z) -> t C(a b c d)
unsafeCoerceP2 = unsafeCoerce#

instance (Show2 a, Show2 b) => Show ( (a :> b) C(x y) ) where
    showsPrec d (x :> y) = showOp2 1 ":>" d x y

instance (Show2 a, Show2 b) => Show2 (a :> b) where
    showsPrec2 = showsPrec

instance (Show2 a, Show2 b) => Show ( (a :\/: b) C(x y) ) where
    showsPrec d (x :\/: y) = showOp2 9 ":\\/:" d x y

instance (Show2 a, Show2 b) => Show2 (a :\/: b) where
    showsPrec2 = showsPrec

infixr 5 :>:, :<:, +>+, +<+

-- forward list
data FL a C(x z) where
    (:>:) :: a C(x y) -> FL a C(y z) -> FL a C(x z)
    NilFL :: FL a C(x x)

instance Show2 a => Show (FL a C(x z)) where
   showsPrec _ NilFL = showString "NilFL"
   showsPrec d (x :>: xs) = showParen (d > prec) $ showsPrec2 (prec + 1) x .
                            showString " :>: " . showsPrec (prec + 1) xs
       where prec = 5

instance Show2 a => Show2 (FL a) where
   showsPrec2 = showsPrec

-- reverse list
data RL a C(x z) where
    (:<:) :: a C(y z) -> RL a C(x y) -> RL a C(x z)
    NilRL :: RL a C(x x)

nullFL :: FL a C(x z) -> Bool
nullFL NilFL = True
nullFL _ = False

nullRL :: RL a C(x z) -> Bool
nullRL NilRL = True
nullRL _ = False

filterFL :: (FORALL(x y) p C(x y) -> EqCheck C(x y)) -> FL p C(w z) -> FL p C(w z)
filterFL _ NilFL = NilFL
filterFL f (x:>:xs) | IsEq <- f x = filterFL f xs
                    | otherwise = x :>: filterFL f xs

filterE :: (a -> EqCheck C(x y)) -> [a] -> [Proof a C(x y)]
filterE _ [] = []
filterE p (x:xs)
    | IsEq <- p x = Proof x : filterE p xs
    | otherwise   = filterE p xs

(+>+) :: FL a C(x y) -> FL a C(y z) -> FL a C(x z)
NilFL +>+ ys = ys
(x:>:xs) +>+ ys = x :>: xs +>+ ys

(+<+) :: RL a C(y z) -> RL a C(x y) -> RL a C(x z)
NilRL +<+ ys = ys
(x:<:xs) +<+ ys = x :<: xs +<+ ys

reverseFL :: FL a C(x z) -> RL a C(x z)
reverseFL xs = r NilRL xs
  where r :: RL a C(l m) -> FL a C(m o) -> RL a C(l o)
        r ls NilFL = ls
        r ls (a:>:as) = r (a:<:ls) as

reverseRL :: RL a C(x z) -> FL a C(x z)
reverseRL xs = r NilFL xs -- r (xs :> NilFL)
  where r :: FL a C(m o) -> RL a C(l m) -> FL a C(l o)
        r ls NilRL = ls
        r ls (a:<:as) = r (a:>:ls) as

concatFL :: FL (FL a) C(x z) -> FL a C(x z)
concatFL NilFL = NilFL
concatFL (a:>:as) = a +>+ concatFL as

concatRL :: RL (RL a) C(x z) -> RL a C(x z)
concatRL NilRL = NilRL
concatRL (a:<:as) = a +<+ concatRL as

spanFL :: (FORALL(w y) a C(w y) -> Bool) -> FL a C(x z) -> (FL a :> FL a) C(x z)
spanFL f (x:>:xs) | f x = case spanFL f xs of
                            ys :> zs -> (x:>:ys) :> zs
spanFL _ xs = NilFL :> xs

splitAtFL :: Int -> FL a C(x z) -> (FL a :> FL a) C(x z)
splitAtFL 0 xs = NilFL :> xs
splitAtFL _ NilFL = NilFL :> NilFL
splitAtFL n (x:>:xs) = case splitAtFL (n-1) xs of
                       (xs':>xs'') -> (x:>:xs' :> xs'')

-- 'bunchFL n' groups patches into batches of n, except that it always puts
-- the first patch in its own group, this being a recognition that the
-- first patch is often *very* large.

bunchFL :: Int -> FL a C(x y) -> FL (FL a) C(x y)
bunchFL _ NilFL = NilFL
bunchFL n (x:>:xs) = (x :>: NilFL) :>: bFL xs
    where bFL :: FL a C(x y) -> FL (FL a) C(x y)
          bFL NilFL = NilFL
          bFL bs = case splitAtFL n bs of
                   a :> b -> a :>: bFL b


allFL :: (FORALL(x y) a C(x y) -> Bool) -> FL a C(w z) -> Bool
allFL f xs = and $ mapFL f xs

foldlFL :: (FORALL(w y) a -> b C(w y) -> a) -> a -> FL b C(x z) -> a
foldlFL _ x NilFL = x
foldlFL f x (y:>:ys) = foldlFL f (f x y) ys

foldlRL :: (FORALL(w y) a -> b C(w y) -> a) -> a -> RL b C(x z) -> a
foldlRL _ x NilRL = x
foldlRL f x (y:<:ys) = foldlRL f (f x y) ys

mapFL_FL :: (FORALL(w y) a C(w y) -> b C(w y)) -> FL a C(x z) -> FL b C(x z)
mapFL_FL _ NilFL = NilFL
mapFL_FL f (a:>:as) = f a :>: mapFL_FL f as

zipWithFL :: (FORALL(x y) a -> p C(x y) -> q C(x y))
          -> [a] -> FL p C(w z) -> FL q C(w z)
zipWithFL f (x:xs) (y :>: ys) = f x y :>: zipWithFL f xs ys
zipWithFL _ _ NilFL = NilFL
zipWithFL _ [] (_:>:_) = error "zipWithFL called with too short a list"

mapRL_RL :: (FORALL(w y) a C(w y) -> b C(w y)) -> RL a C(x z) -> RL b C(x z)
mapRL_RL _ NilRL = NilRL
mapRL_RL f (a:<:as) = f a :<: mapRL_RL f as

mapFL :: (FORALL(w z) a C(w z) -> b) -> FL a C(x y) -> [b]
mapFL _ NilFL = []
mapFL f (a :>: b) = f a : mapFL f b

mapRL :: (FORALL(w z) a C(w z) -> b) -> RL a C(x y) -> [b]
mapRL _ NilRL = []
mapRL f (a :<: b) = f a : mapRL f b

lengthFL :: FL a C(x z) -> Int
lengthFL xs = l xs 0
  where l :: FL a C(x z) -> Int -> Int
        l NilFL n = n
        l (_:>:as) n = l as $! n+1

lengthRL :: RL a C(x z) -> Int
lengthRL xs = l xs 0
  where l :: RL a C(x z) -> Int -> Int
        l NilRL n = n
        l (_:<:as) n = l as $! n+1

isShorterThanRL :: RL a C(x y) -> Int -> Bool
isShorterThanRL _ n | n <= 0 = False
isShorterThanRL NilRL _ = True
isShorterThanRL (_:<:xs) n = isShorterThanRL xs (n-1)

concatReverseFL :: FL (RL a) C(x y) -> RL a C(x y)
concatReverseFL = concatRL . reverseFL

consRLSealed :: a C(y z) -> FlippedSeal (RL a) C(y) -> FlippedSeal (RL a) C(z)
consRLSealed a (FlippedSeal as) = flipSeal $ a :<: as

#ifndef GADT_WITNESSES
-- These are useful for interfacing with modules outside of
-- patch theory, such as Show.lhs
unsafeUnFL :: FL a -> [a]
unsafeUnFL NilFL = []
unsafeUnFL (a:>:as) = a : unsafeUnFL as

unsafeUnRL :: RL a -> [a]
unsafeUnRL NilRL = []
unsafeUnRL (a:<:as) = a : unsafeUnRL as

unsafeFL :: [a] -> FL a
unsafeFL [] = NilFL
unsafeFL (a:as) = a :>: unsafeFL as

unsafeRL :: [a] -> RL a
unsafeRL [] = NilRL
unsafeRL (a:as) = a :<: unsafeRL as
#endif
