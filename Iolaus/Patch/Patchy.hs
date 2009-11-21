-- Copyright (C) 2007,2009 David Roundy
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
-- , TypeOperators, GADTs #-}

#include "gadts.h"

module Iolaus.Patch.Patchy ( Patchy,
                            Apply, apply,
                            Commute(..), commuteFL, commuteRL, commuteRLFL,
                            ShowPatch(..),
                            Invert(..), invertFL, invertRL ) where

import Data.List ( nub )

import Iolaus.SlurpDirectory ( Slurpy )
import Iolaus.Ordered
import Iolaus.Printer ( Doc )
import Iolaus.IO ( WriteableDirectory )
import Iolaus.English ( plural, Noun(Noun) )

--import Iolaus.ColorPrinter ( traceDoc )
--import Iolaus.Printer ( greenText, ($$) )

class (Apply p, Commute p, ShowPatch p, Invert p) => Patchy p where
-- instance (ShowPatch p, Invert p) => Patchy p where

class Apply p where
    apply :: WriteableDirectory m => p C(x y) -> m ()

class Commute p where
    commute :: (p :> p) C(x y) -> Maybe ((p :> p) C(x y))
    merge :: (p :\/: p) C(x y) -> Maybe ((p :/\: p) C(x y))
    list_touched_files :: p C(x y) -> [FilePath]

class Commute p => ShowPatch p where
    showPatch :: p C(x y) -> Doc
    showContextPatch :: Slurpy C(x) -> p C(x y) -> Doc
    showContextPatch _ p = showPatch p
    thing :: p C(x y) -> String
    thing _ = "patch"
    things :: p C(x y) -> String
    things x = plural (Noun $ thing x) ""

class MyEq p => Invert p where
    invert :: p C(x y) -> p C(y x)
    identity :: p C(x x)
    sloppyIdentity :: p C(x y) -> EqCheck C(x y)
    sloppyIdentity p = identity =\/= p

instance Apply p => Apply (FL p) where
    apply NilFL = return ()
    apply (p:>:ps) = apply p >> apply ps

instance Commute p => Commute (FL p) where
    commute (NilFL :> x) = Just (x :> NilFL)
    commute (x :> NilFL) = Just (NilFL :> x)
    commute (xs :> ys) = do ys' :> rxs' <- commuteRLFL (reverseFL xs :> ys)
                            return $ ys' :> reverseRL rxs'
    merge (NilFL :\/: x) = Just (x :/\: NilFL)
    merge (x :\/: NilFL) = Just (NilFL :/\: x)
    merge ((x:>:xs) :\/: ys) = do ys' :/\: x' <- mergeFL (x :\/: ys)
                                  xs' :/\: ys'' <- merge (ys' :\/: xs)
                                  Just (ys'' :/\: (x' :>: xs'))
    list_touched_files xs = nub $ concat $ mapFL list_touched_files xs

mergeFL :: Commute p => (p :\/: FL p) C(x y) -> Maybe ((FL p :/\: p) C(x y))
mergeFL (p :\/: NilFL) = Just (NilFL :/\: p)
mergeFL (p :\/: (x :>: xs)) = do x' :/\: p' <- merge (p :\/: x)
                                 xs' :/\: p'' <- mergeFL (p' :\/: xs)
                                 Just ((x' :>: xs') :/\: p'')

commuteRLFL :: Commute p => (RL p :> FL p) C(x y) -> Maybe ((FL p :> RL p) C(x y))
commuteRLFL (NilRL :> ys) = Just (ys :> NilRL)
commuteRLFL (xs :> NilFL) = Just (NilFL :> xs)
commuteRLFL (xs :> y :>: ys) = do y' :> xs' <- commuteRL (xs :> y)
                                  ys' :> xs'' <- commuteRLFL (xs' :> ys)
                                  return (y' :>: ys' :> xs'')

commuteRL :: Commute p => (RL p :> p) C(x y) -> Maybe ((p :> RL p) C(x y))
commuteRL (z :<: zs :> w) = do w' :> z' <- commute (z :> w)
                               w'' :> zs' <- commuteRL (zs :> w')
                               return (w'' :> z' :<: zs')
commuteRL (NilRL :> w) = Just (w :> NilRL)

commuteFL :: Commute p => (p :> FL p) C(x y) -> Maybe ((FL p :> p) C(x y))
commuteFL (p :> NilFL) = Just (NilFL :> p)
commuteFL (q :> p :>: ps) = do p' :> q' <- commute (q :> p)
                               ps' :> q'' <- commuteFL (q' :> ps)
                               Just (p' :>: ps' :> q'')

instance Apply p => Apply (RL p) where
    apply NilRL = return ()
    apply (p:<:ps) = apply ps >> apply p
instance Commute p => Commute (RL p) where
    commute (xs :> ys) = do fys' :> xs' <- commuteRLFL (xs :> reverseRL ys)
                            return (reverseFL fys' :> xs')
    merge (x :\/: y) = do ry' :/\: rx' <- merge (reverseRL x :\/: reverseRL y)
                          Just $ reverseFL ry' :/\: reverseFL rx'
    list_touched_files = list_touched_files . reverseRL

invertFL :: Invert p => FL p C(x y) -> RL p C(y x)
invertFL NilFL = NilRL
invertFL (x:>:xs) = invert x :<: invertFL xs

invertRL :: Invert p => RL p C(x y) -> FL p C(y x)
invertRL NilRL = NilFL
invertRL (x:<:xs) = invert x :>: invertRL xs
