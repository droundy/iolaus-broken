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
{-# LANGUAGE CPP #-}
#include "gadts.h"

module Iolaus.SlurpDirectory
 (Slurpy, empty_slurpy, slurp, filterSlurpyPaths,
  get_filehash, get_dirhash, get_fileEbit,
  co_slurp, slurp_unboring, subslurpies,
  doesFileReallyExist, doesDirectoryReallyExist, isFileReallySymlink,
  is_dir, is_file,
  get_slurp, slurp_name,
  slurp_hasdir, slurp_has, slurp_has_anycase, slurp_hasfile,
  list_slurpy, list_slurpy_files, list_slurpy_dirs, get_path_list,
  get_filecontents, get_dircontents,
  slurp_modfile, slurp_remove, slurp_removefile, slurp_removedir,
  write_files,
  SlurpMonad, withSlurpy, writeSlurpy
 )
 where

import Iolaus.SlurpDirectoryInternal
 (Slurpy, empty_slurpy, slurp, subslurpies,
  get_filehash, get_dirhash, get_fileEbit,
  co_slurp, slurp_unboring,
  doesFileReallyExist, doesDirectoryReallyExist, isFileReallySymlink,
  is_dir, is_file,
  get_slurp, slurp_name, slurp_has_anycase,
  list_slurpy, list_slurpy_files, list_slurpy_dirs, get_path_list,
  get_filecontents, get_dircontents,
  slurp_modfile, write_files,
  SlurpMonad, withSlurpy, writeSlurpy )

import qualified Iolaus.SlurpDirectoryInternal as SDI
import Iolaus.FileName ( fp2fn )
import Iolaus.RepoPath ( FilePathLike( toFilePath ) )

slurp_has :: FilePathLike p => p -> Slurpy C(x) -> Bool
slurp_has f s = SDI.slurp_has (fp2fn $ toFilePath f) s

slurp_hasfile :: FilePathLike p => p -> Slurpy C(x) -> Bool
slurp_hasfile f s = SDI.slurp_hasfile (fp2fn $ toFilePath f) s

slurp_hasdir :: FilePathLike p => p -> Slurpy C(x) -> Bool
slurp_hasdir f s = SDI.slurp_hasdir (fp2fn $ toFilePath f) s

slurp_remove :: FilePathLike p => p -> Slurpy C(x) -> Maybe (Slurpy C(x))
slurp_remove f s = SDI.slurp_remove (fp2fn $ toFilePath f) s

slurp_removedir :: FilePathLike p => p -> Slurpy C(x) -> Maybe (Slurpy C(x))
slurp_removedir f s = SDI.slurp_removedir (fp2fn $ toFilePath f) s

slurp_removefile :: FilePathLike p => p -> Slurpy C(x) -> Maybe (Slurpy C(x))
slurp_removefile f s = SDI.slurp_removefile (fp2fn $ toFilePath f) s

filterSlurpyPaths :: FilePathLike p => [p] -> Slurpy C(x) -> Slurpy C(x)
filterSlurpyPaths f = SDI.filterSlurpyPaths (map (fp2fn . toFilePath) f)
