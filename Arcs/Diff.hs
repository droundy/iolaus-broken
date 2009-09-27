-- Copyright (C) 2002-2003,2009 David Roundy
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

{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Arcs.Diff ( unsafeDiff ) where

#ifndef GADT_WITNESSES
import Data.List ( intersperse )
import Arcs.ByteStringUtils ( linesPS)
import qualified Data.ByteString.Char8 as BC (last)
import qualified Data.ByteString as B (empty, ByteString)
#endif

import Arcs.SlurpDirectory ( Slurpy, slurp_name, is_dir, is_file,
                             get_filehash, get_dirhash, get_fileEbit,
                             get_dircontents, get_filecontents,
#ifndef GADT_WITNESSES
                             FileContents
#endif
                           )
import Arcs.IO ( ExecutableBit(..) )
import Arcs.Patch ( Prim
#ifndef GADT_WITNESSES
                   , hunk, canonize, rmfile, rmdir
                   , addfile, adddir, chmod, invert
#endif
                   )
import Arcs.Flags ( ArcsFlag(..) )
import Arcs.Ordered ( FL(..)
#ifndef GADT_WITNESSES
                           , (+>+)
#endif
                           )
#ifndef GADT_WITNESSES
#include "impossible.h"
#endif

#ifndef GADT_WITNESSES
diff :: Bool -> Slurpy -> Slurpy -> FL Prim
diff summary s1 s2 = gendiff summary [] s1 s2 NilFL

#endif

-- The diff function takes a recursive diff of two slurped-up directory trees.

unsafeDiff :: [ArcsFlag]
           -> Slurpy -> Slurpy -> FL Prim C(x y)
#ifdef GADT_WITNESSES
unsafeDiff = undefined
#else
unsafeDiff opts s1 s2 = diff summary s1 s2
  where -- NoSummary/Summary both present gives False
        -- Just Summary gives True
        -- Just NoSummary gives False
        -- Neither gives False
        summary = Summary `elem` opts && NoSummary `notElem` opts

mk_filepath :: [FilePath] -> FilePath
mk_filepath fps = concat $ intersperse "/" $ reverse fps

gendiff :: Bool -> [FilePath] -> Slurpy -> Slurpy
        -> (FL Prim -> FL Prim)
gendiff summary fps s1 s2
    | get_dirhash s1 == get_dirhash s2 && get_dirhash s1 /= Nothing = id
    | is_file s1 && is_file s2 = diff_regular_files f s1 s2
    | is_dir s1 && is_dir s2 = fps' `seq` recur_diff summary fps' dc1 dc2
    | otherwise = id
    where n2 = slurp_name s2
          f = mk_filepath (n2:fps)
          dc1 = get_dircontents s1
          dc2 = get_dircontents s2
          fps' = case n2 of "." -> fps
                            _ -> n2:fps

-- recur_diff or recursive diff
-- First parameter is Summary?
recur_diff :: Bool
           -> [FilePath] -> [Slurpy] -> [Slurpy]
           -> (FL Prim -> FL Prim)
recur_diff _ _ [] [] = id
recur_diff summary fps (s:ss) (s':ss')
    -- this is the case if a file has been removed in the working directory
    | s < s' = diff_removed fps s . recur_diff summary fps ss (s':ss')
    -- this next case is when there is a file in the directory that is not
    -- in the repository (ie, not managed by darcs)
    | s > s' = diff_added summary fps s' . recur_diff summary fps (s:ss) ss'
    -- actually compare the files because the names match
    | s == s' = gendiff summary fps s s' . recur_diff summary fps ss ss'
recur_diff opts fps (s:ss) [] = diff_removed fps s . recur_diff opts fps ss []
recur_diff summary fps [] (s':ss') =
    diff_added summary fps s' . recur_diff summary fps [] ss'
recur_diff _ _ _ _ = impossible

-- diff, taking into account paranoidness and file type, two regular files
diff_regular_files :: FilePath -> Slurpy -> Slurpy -> (FL Prim -> FL Prim)
diff_regular_files f s1 s2 = 
    if maybe_differ   
        then chm . diff_files f b1 b2
        else chm . id
  where maybe_differ = get_filehash s1 /= get_filehash s2
                     || get_filehash s1 == Nothing
        b1 = get_filecontents s1
        b2 = get_filecontents s2
        chm = case (get_fileEbit s1, get_fileEbit s2) of
              (Just IsExecutable, Just NotExecutable) ->
                  (chmod f NotExecutable :>:)
              (Just NotExecutable, Just IsExecutable) ->
                  (chmod f IsExecutable :>:)
              _ -> id

-- creates a diff for a file or directory which needs to be added to the
-- repository
diff_added :: Bool -> [FilePath] -> Slurpy
           -> (FL Prim -> FL Prim)
diff_added summary fps s
    | is_file s = (addfile f:>:) .
                  (if summary then id
                              else diff_from_empty id f (get_filecontents s))
    | otherwise {- is_dir s -} =
        (adddir f:>:)
      . foldr (.) id (map (diff_added summary (n:fps)) (get_dircontents s))
    where n = slurp_name s
          f = mk_filepath (n:fps)

get_text :: FileContents -> [B.ByteString]
get_text = linesPS

empt :: FileContents
empt = B.empty

diff_files :: FilePath -> FileContents -> FileContents
           -> (FL Prim -> FL Prim)
diff_files f o n | get_text o == [B.empty] && get_text n == [B.empty] = id
                 | get_text o == [B.empty] = diff_from_empty id f n
                 | get_text n == [B.empty] = diff_from_empty invert f o
diff_files f o n = if o == n
                   then id
                   else (canonize (hunk f 1 (linesPS o) (linesPS n)) +>+)

diff_from_empty :: (Prim -> Prim) -> FilePath -> FileContents
                -> (FL Prim -> FL Prim)
diff_from_empty inv f b =
    if b == B.empty
    then id
    else let p = if BC.last b == '\n'
                 then hunk f 1 [] $ init $ linesPS b
                 else hunk f 1 [B.empty] $ linesPS b
         in (inv p:>:)
#endif

#ifndef GADT_WITNESSES
diff_removed :: [FilePath] -> Slurpy -> (FL Prim -> FL Prim)
diff_removed fps s
    | is_file s = diff_files f (get_filecontents s) empt . (rmfile f:>:)
    | otherwise {- is_dir s -}
        = foldr (.) (rmdir f:>:)
        $ map (diff_removed (n:fps)) (get_dircontents s)
    where n = slurp_name s
          f = mk_filepath (n:fps)
#endif
