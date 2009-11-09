-- Copyright (C) 2002-2004 David Roundy
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

module Iolaus.TouchesFiles ( look_touch, choose_touching,
                      select_touching,
                      deselect_not_touching, select_not_touching,
                    ) where

import Iolaus.PatchChoices ( PatchChoices, Tag, TaggedPatch,
                             patch_choices, tag, get_choices,
                      force_firsts, force_lasts, tp_patch,
                    )
import Iolaus.Patch ( Patchy, list_touched_files )
import Iolaus.Ordered ( FL(..), (:>)(..), mapFL_FL, (+>+) )
import Iolaus.Sealed ( Sealed, seal )

select_touching :: Patchy p => [FilePath] -> PatchChoices p C(x y) -> PatchChoices p C(x y)
select_touching [] pc = pc
select_touching files pc = force_firsts xs pc
    where ct :: Patchy p => [FilePath] -> FL (TaggedPatch p) C(x y) -> [Tag]
          ct _ NilFL = []
          ct fs (tp:>:tps) = case look_touch0 fs (tp_patch tp) of
                             (True, fs') -> tag tp:ct fs' tps
                             (False, fs') -> ct fs' tps
          xs = case get_choices pc of
               _ :> mc :> lc -> ct (map fix files) (mc +>+ lc)

deselect_not_touching :: Patchy p => [FilePath] -> PatchChoices p C(x y) -> PatchChoices p C(x y)
deselect_not_touching [] pc = pc
deselect_not_touching files pc = force_lasts xs pc
    where ct :: Patchy p => [FilePath] -> FL (TaggedPatch p) C(x y) -> [Tag]
          ct _ NilFL = []
          ct fs (tp:>:tps) = case look_touch0 fs (tp_patch tp) of
                             (True, fs') -> ct fs' tps
                             (False, fs') -> tag tp:ct fs' tps
          xs = case get_choices pc of
               fc :> mc :> _ -> ct (map fix files) (fc +>+ mc)

select_not_touching :: Patchy p => [FilePath] -> PatchChoices p C(x y) -> PatchChoices p C(x y)
select_not_touching [] pc = pc
select_not_touching files pc = force_firsts xs pc
    where ct :: Patchy p => [FilePath] -> FL (TaggedPatch p) C(x y) -> [Tag]
          ct _ NilFL = []
          ct fs (tp:>:tps) = case look_touch0 fs (tp_patch tp) of
                             (True, fs') -> ct fs' tps
                             (False, fs') -> tag tp:ct fs' tps
          xs = case get_choices pc of
               fc :> mc :> _ -> ct (map fix files) (fc +>+ mc)

fix :: FilePath -> FilePath
fix ('.':'/':x) = fix0 x
fix x = fix0 x

fix0 :: FilePath -> FilePath
fix0 f | take 1 (reverse f) == "/" = fix $ reverse $ drop 1 $ reverse f
fix0 "" = "."
fix0 "." = "."
fix0 f = "./" ++ f

choose_touching :: Patchy p => [FilePath] -> FL p C(x y) -> Sealed (FL p C(x))
choose_touching [] p = seal p
choose_touching files p = case get_choices $ select_touching files $ patch_choices p of
                          fc :> _ :> _ -> seal $ mapFL_FL tp_patch fc

look_touch :: Patchy p => [FilePath] -> p C(x y) -> Bool
look_touch fs p = fst $ look_touch0 fs p

look_touch0 :: Patchy p => [FilePath] -> p C(x y) -> (Bool, [FilePath])
look_touch0 fs0 p = (any (\tf -> any (affects tf) fs)
                         (map fix $ list_touched_files p)
                    || fs' /= fs, fs')
    where affects touched f | touched == f = True
          affects t f = case splitAt (length f) t of
                        (t', '/':_) -> t' == f
                        _ -> case splitAt (length t) f of
                             (f', '/':_) -> f' == t
                             _ -> False
          fs = map fix fs0
          fs' = fs -- sort $ apply_to_filepaths p fs
