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
-- , MagicHash, GADTs #-}

#include "gadts.h"

module Arcs.Sealed ( Sealed(..), seal, unseal, mapSeal,
#ifndef GADT_WITNESSES
                      unsafeUnseal, unsafeUnflippedseal, unsafeUnseal2,
#endif
                      Sealed2(..), seal2, unseal2, mapSeal2,
                      FlippedSeal(..), flipSeal, unsealFlipped, mapFlipped,
                      unsealM, liftSM
                    ) where

import GHC.Base ( unsafeCoerce# )
import Arcs.Show

data Sealed a where
    Sealed :: a C(x ) -> Sealed a

seal :: a C(x ) -> Sealed a
seal = Sealed

data Sealed2 a where
    Sealed2 :: !(a C(x y )) -> Sealed2 a

seal2 :: a C(x y ) -> Sealed2 a
seal2 = Sealed2

data FlippedSeal a C(y) where
    FlippedSeal :: !(a C(x y)) -> FlippedSeal a C(y)

flipSeal :: a C(x y) -> FlippedSeal a C(y)
flipSeal = FlippedSeal

#ifndef GADT_WITNESSES
unsafeUnseal :: Sealed a -> a
unsafeUnseal (Sealed a) = a

unsafeUnflippedseal :: FlippedSeal a -> a
unsafeUnflippedseal (FlippedSeal a) = a

unsafeUnseal2 :: Sealed2 a -> a
unsafeUnseal2 (Sealed2 a) = a
#endif

seriouslyUnsafeUnseal :: Sealed a -> a C(())
seriouslyUnsafeUnseal (Sealed a) = unsafeCoerce# a

unseal :: (FORALL(x) a C(x ) -> b) -> Sealed a -> b
unseal f x = f (seriouslyUnsafeUnseal x)

-- laziness property:
-- unseal (const True) undefined == True

unsealM :: Monad m => m (Sealed a) -> (FORALL(x) a C(x) -> m b) -> m b
unsealM m1 m2 = do sx <- m1
                   unseal m2 sx

liftSM :: Monad m => (FORALL(x) a C(x) -> b) -> m (Sealed a) -> m b
liftSM f m = do sx <- m
                return (unseal f sx)

mapSeal :: (FORALL(x) a C(x ) -> b C(x )) -> Sealed a -> Sealed b
mapSeal f = unseal (seal . f)

mapFlipped :: (FORALL(x) a C(x y) -> b C(x z)) -> FlippedSeal a C(y) -> FlippedSeal b C(z)
mapFlipped f (FlippedSeal x) = FlippedSeal (f x)

seriouslyUnsafeUnseal2 :: Sealed2 a -> a C(() ())
seriouslyUnsafeUnseal2 (Sealed2 a) = unsafeCoerce# a

unseal2 :: (FORALL(x y) a C(x y ) -> b) -> Sealed2 a -> b
unseal2 f a = f (seriouslyUnsafeUnseal2 a)

mapSeal2 :: (FORALL(x y) a C(x y ) -> b C(x y )) -> Sealed2 a -> Sealed2 b
mapSeal2 f = unseal2 (seal2 . f)

unsealFlipped :: (FORALL(x y) a C(x y) -> b) -> FlippedSeal a C(z) -> b
unsealFlipped f (FlippedSeal a) = f a

instance Show1 a => Show (Sealed a) where
    showsPrec d (Sealed x) = showParen (d > app_prec) $ showString "Sealed " . showsPrec1 (app_prec + 1) x
instance Show2 a => Show (Sealed2 a) where
    showsPrec d (Sealed2 x) = showParen (d > app_prec) $ showString "Sealed2 " . showsPrec2 (app_prec + 1) x
