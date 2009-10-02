--  Copyright (C) 2009 David Roundy
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
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Iolaus.Repository ( get_unrecorded_changes,
                           slurp_recorded, slurp_working ) where

import Iolaus.FileName ( fp2fn )
import Iolaus.Diff ( unsafeDiff )
import Iolaus.Patch ( Prim )
import Iolaus.Ordered ( FL )
import Iolaus.SlurpDirectory ( Slurpy, empty_slurpy )

import Git.Plumbing ( heads, writetree,
                      updateindex, catCommitTree )
import Git.Helpers ( touchedFiles, slurpTree )

slurp_recorded :: IO Slurpy
slurp_recorded =
    do hs <- heads
       case hs of
         [] -> return empty_slurpy -- no history!
         [h] -> catCommitTree h >>= slurpTree (fp2fn ".")
         _ -> fail "can't yet handle multiple-head case"

slurp_working :: IO Slurpy
slurp_working =
    do touchedFiles >>= updateindex
       writetree >>= slurpTree (fp2fn ".")

get_unrecorded_changes :: IO (FL Prim)
get_unrecorded_changes =
    do new <- slurp_working
       old <- slurp_recorded
       return $ unsafeDiff [] old new
