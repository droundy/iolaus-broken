-- Copyright (C) 2003 David Roundy
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

{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Iolaus.PrintPatch ( printPatch, contextualPrintPatch,
                         printPatchPager, printFriendly ) where

import Iolaus.Patch ( Patchy, Effect, showContextPatch, showPatch, summarize )
import Iolaus.Flags ( Flag( Summary ) )
import Iolaus.SlurpDirectory ( Slurpy )
import Iolaus.Printer ( putDocLnWith )
import Iolaus.ColorPrinter ( fancyPrinters )

-- | @'printFriendly' opts patch@ prints @patch@ in accordance with the
-- flags in opts, ie, whether @--verbose@ or @--summary@ were passed at
-- the command-line.
printFriendly :: (Effect p, Patchy p) => [Flag] -> p C(x y) -> IO ()
printFriendly opts p = putDocLnWith fancyPrinters $ if Summary `elem` opts
                                                    then summarize p
                                                    else showPatch p

-- | 'printPatch' prints a patch on standard output.
printPatch :: Patchy p => p C(x y) -> IO ()
printPatch p = putDocLnWith fancyPrinters $ showPatch p

-- | 'printPatchPager' runs '$PAGER' and shows a patch in it.
printPatchPager :: Patchy p => p C(x y) -> IO ()
printPatchPager = printPatch

-- | 'contextualPrintPatch' prints a patch, together with its context,
-- on standard output.
contextualPrintPatch :: Patchy p => Slurpy C(x) -> p C(x y) -> IO ()
contextualPrintPatch s p = putDocLnWith fancyPrinters $ showContextPatch s p
