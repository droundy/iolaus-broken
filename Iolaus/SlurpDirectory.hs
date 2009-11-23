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
 (Slurpy, empty_slurpy, filterSlurpyPaths,
  get_filehash, get_dirhash, get_fileEbit, subslurpies,
  doesFileReallyExist, doesDirectoryReallyExist,
  is_dir, is_file,
  get_slurp, slurp_name,
  list_slurpy, list_slurpy_files, list_slurpy_dirs, get_path_list,
  get_filecontents, get_dircontents,
  SlurpMonad, withSlurpy,
 )
 where

import Iolaus.SlurpDirectoryInternal
 (Slurpy, empty_slurpy, subslurpies,
  get_filehash, get_dirhash, get_fileEbit,
  doesFileReallyExist, doesDirectoryReallyExist,
  is_dir, is_file,
  get_slurp, slurp_name,
  list_slurpy, list_slurpy_files, list_slurpy_dirs, get_path_list,
  get_filecontents, get_dircontents,
  SlurpMonad, withSlurpy )

import qualified Iolaus.SlurpDirectoryInternal as SDI
import Iolaus.FileName ( fp2fn )
import Iolaus.RepoPath ( FilePathLike( toFilePath ) )

filterSlurpyPaths :: FilePathLike p => [p] -> Slurpy C(x) -> Slurpy C(x)
filterSlurpyPaths f = SDI.filterSlurpyPaths (map (fp2fn . toFilePath) f)
