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

module Iolaus.Diff ( unsafeDiff ) where

import Data.List ( partition, sort )
import Iolaus.Lcs2 ( patientLcs )
#ifndef GADT_WITNESSES
import Data.List ( intersperse )
import Iolaus.ByteStringUtils ( linesPS)
import qualified Data.ByteString.Char8 as BC (last)
import qualified Data.ByteString as B (empty, ByteString)
#endif

import Iolaus.SlurpDirectory ( Slurpy, slurp_name, is_dir, is_file,
                             get_filehash, get_dirhash, get_fileEbit,
                             get_dircontents, get_filecontents )
import Iolaus.IO ( ExecutableBit(..) )
import Iolaus.Patch ( Prim, apply_to_slurpy, move
#ifndef GADT_WITNESSES
                   , hunk, canonize, rmfile, rmdir
                   , addfile, adddir, chmod, invert
#endif
                   )
import Iolaus.Flags ( IolausFlag(..) )
import Iolaus.Ordered ( FL(..)
#ifndef GADT_WITNESSES
                           , (+>+)
#endif
                           )
#ifndef GADT_WITNESSES
#include "impossible.h"
#endif

-- | take a recursive diff of two slurped-up directory trees.

unsafeDiff :: [IolausFlag]
           -> Slurpy -> Slurpy -> FL Prim C(x y)
#ifdef GADT_WITNESSES
unsafeDiff = undefined
#else
unsafeDiff opts s1 s2 = find_mvs xs0
  where summary = Summary `elem` opts && NoSummary `notElem` opts
        (xs0,ys) = addedremoved s1 s2
        find_mvs (x:xs) =
            case filter (similar (snd x) . snd) ys of
              y:_ -> case apply_to_slurpy p s1 of
                     Just s1' -> p :>: unsafeDiff opts s1' s2
                     Nothing ->
                         case apply_to_slurpy (invert p) s2 of
                         Just s2' -> unsafeDiff opts s1 s2' +>+ p :>: NilFL
                         Nothing -> find_mvs xs -- yikes
                  where p = move (fst x) (fst y)
              [] -> find_mvs xs
        find_mvs [] = gendiff summary [] s1 s2 NilFL

mk_filepath :: [FilePath] -> FilePath
mk_filepath fps = concat $ intersperse "/" $ reverse fps

addedremoved :: Slurpy -> Slurpy -> ([(FilePath,Slurpy)],[(FilePath,Slurpy)])
addedremoved o n
    | get_dirhash o == get_dirhash n && get_dirhash o /= Nothing = ([],[])
    | is_file o && is_file n = ([],[])
    | is_dir o && is_dir n = addrm (get_dircontents o) (get_dircontents n)
    | otherwise = ([],[])
    where addrm [] xs = ([], map (\x -> (slurp_name x,x)) xs)
          addrm xs [] = (map (\x -> (slurp_name x,x)) xs, [])
          addrm (s:xs) ys =
              case partition ((==slurp_name s).slurp_name) ys of
                ([],_) -> case addrm xs ys of
                            (a,b) -> ((slurp_name s,s):a, b)
                ([s'],ys') ->
                    case addedremoved s s' of
                      (a, b) -> case addrm xs ys' of
                                  (a',b') ->
                                      (a' ++ map fixit a, b' ++ map fixit b)
                          where fixit (f1,s1) =
                                    (mk_filepath [f1,slurp_name s], s1)
                _ -> impossible

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
                  (if get_fileEbit s == Just IsExecutable
                   then (chmod f IsExecutable :>:)
                   else id) .
                  (if summary then id
                              else diff_from_empty id f (get_filecontents s))
    | otherwise {- is_dir s -} =
        (adddir f:>:)
      . foldr (.) id (map (diff_added summary (n:fps)) (get_dircontents s))
    where n = slurp_name s
          f = mk_filepath (n:fps)

diff_files :: FilePath -> B.ByteString -> B.ByteString
           -> (FL Prim -> FL Prim)
diff_files f o n | linesPS o == [B.empty] && linesPS n == [B.empty] = id
                 | linesPS o == [B.empty] = diff_from_empty id f n
                 | linesPS n == [B.empty] = diff_from_empty invert f o
diff_files f o n = if o == n
                   then id
                   else (canonize (hunk f 1 (linesPS o) (linesPS n)) +>+)

diff_from_empty :: (Prim -> Prim) -> FilePath -> B.ByteString
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
    | is_file s = diff_files f (get_filecontents s) B.empty . (rmfile f:>:)
    | otherwise {- is_dir s -}
        = foldr (.) (rmdir f:>:)
        $ map (diff_removed (n:fps)) (get_dircontents s)
    where n = slurp_name s
          f = mk_filepath (n:fps)
#endif

similar :: Slurpy -> Slurpy -> Bool
similar aaa bbb
    | afile /= bfile = False
    | afile = length (patientLcs (linesPS $ get_filecontents aaa)
                                 (linesPS $ get_filecontents bbb))
              > 40
    | otherwise = compared (sort $ get_dircontents aaa)
                           (sort $ get_dircontents bbb)
    where afile = is_file aaa
          bfile = is_file bbb
          compared (a:as) (b:bs)
              | slurp_name a == slurp_name b = similar a b || compared as bs
              | a < b = compared as (b:bs)
              | otherwise = compared (a:as) bs
          compared [] _ = False
          compared _ [] = False
