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
{-# LANGUAGE CPP #-}
#include "gadts.h"

module Iolaus.Repository ( write_head,
                           get_unrecorded_changes,
                           get_unrecorded, Unrecorded(..),
                           slurp_recorded, slurp_working ) where

import Control.Monad ( zipWithM_ )

import Iolaus.Diff ( diff )
import Iolaus.Patch ( Prim )
import Iolaus.Flags ( IolausFlag )
import Iolaus.Ordered ( FL, unsafeCoerceS )
import Iolaus.SlurpDirectory ( Slurpy )
import Iolaus.Sealed ( Sealed(..), mapSealM )

import Git.Plumbing ( Hash, Commit, heads, writetree, updateindex, updateref )
import Git.Helpers ( touchedFiles, slurpTree, mergeCommits )
import Git.Dag ( cauterizeHeads )

slurp_recorded :: [IolausFlag] -> IO (Slurpy C(RecordedState))
slurp_recorded opts = do Sealed r <- heads >>= mergeCommits opts
                         slurpTree $ unsafeCoerceS r

slurp_working :: IO (Sealed Slurpy)
slurp_working =
    do touchedFiles >>= updateindex
       writetree >>= mapSealM slurpTree

data RecordedState = RecordedState

data Unrecorded =
    FORALL(x) Unrecorded (FL Prim C(RecordedState x)) (Slurpy C(x))

get_unrecorded :: [IolausFlag] -> IO Unrecorded
get_unrecorded opts =
    do Sealed new <- slurp_working
       old <- slurp_recorded opts
       return $ Unrecorded (diff [] old new) new

get_unrecorded_changes :: [IolausFlag] -> IO (Sealed (FL Prim C(RecordedState)))
get_unrecorded_changes opts =
    do Sealed new <- slurp_working
       old <- slurp_recorded opts
       return $ Sealed $ diff [] old new

write_head :: Hash Commit C(x) -> IO ()
write_head h =
    do hs <- heads
       case cauterizeHeads (Sealed h:hs) of
         [Sealed h'] -> updateref "refs/heads/master" h'
         hs' -> zipWithM_ (\mm (Sealed hh) -> updateref mm hh) masters hs'
    where masters = "refs/heads/master" :
                    map (\n -> "refs/heads/master"++show n) [1 :: Int ..]

