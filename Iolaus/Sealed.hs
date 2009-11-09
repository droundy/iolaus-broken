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

module Iolaus.Sealed ( Sealed(..), seal, unseal, mapSeal, mapSealM,
                       FlippedSeal(..), flipSeal ) where

import GHC.Base ( unsafeCoerce# )
import Iolaus.Show

data Sealed a where
    Sealed :: a C(x ) -> Sealed a

seal :: a C(x ) -> Sealed a
seal = Sealed

data FlippedSeal a C(y) where
    FlippedSeal :: !(a C(x y)) -> FlippedSeal a C(y)

flipSeal :: a C(x y) -> FlippedSeal a C(y)
flipSeal = FlippedSeal

seriouslyUnsafeUnseal :: Sealed a -> a C(())
seriouslyUnsafeUnseal (Sealed a) = unsafeCoerce# a

unseal :: (FORALL(x) a C(x ) -> b) -> Sealed a -> b
unseal f x = f (seriouslyUnsafeUnseal x)

-- laziness property:
-- unseal (const True) undefined == True

mapSealM :: Monad m => (FORALL(x) a C(x) -> m (b C(x))) -> Sealed a -> m (Sealed b)
mapSealM f (Sealed x) = do y <- f x
                           return (Sealed y)

mapSeal :: (FORALL(x) a C(x ) -> b C(x )) -> Sealed a -> Sealed b
mapSeal f = unseal (seal . f)

instance Show1 a => Show (Sealed a) where
    showsPrec d (Sealed x) = showsPrec1 d x
instance Eq1 a => Eq (Sealed a) where
    Sealed x == Sealed y = eq1 x y
instance Ord1 a => Ord (Sealed a) where
    compare (Sealed x) (Sealed y) = compare1 x y
instance Pretty1 a => Pretty (Sealed a) where
    pretty = unseal pretty1
