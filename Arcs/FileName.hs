-- Copyright (C) 2002-2003 David Roundy
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

-- | FileName is an abstract type intended to facilitate the input and output of
-- unicode filenames.
module Arcs.FileName ( FileName( ),
                        fp2fn, fn2fp,
                        break_on_dir, norm_path, own_name, super_name,
                        movedirfilename,
                        encode_white, decode_white,
                        (///),
                        breakup
                      ) where

import System.IO
import Data.Char ( isSpace, chr, ord )

newtype FileName = FN FilePath deriving ( Eq, Ord )

instance Show FileName where
   showsPrec d (FN fp) = showParen (d > app_prec) $ showString "fp2fn " . showsPrec (app_prec + 1) fp
      where app_prec = 10

{-# INLINE fp2fn #-}
fp2fn :: FilePath -> FileName
fp2fn fp = FN fp

{-# INLINE fn2fp #-}
fn2fp :: FileName -> FilePath
fn2fp (FN fp) = fp

encode_white :: FilePath -> String
encode_white (c:cs) | isSpace c || c == '\\' =
    '\\' : (show $ ord c) ++ "\\" ++ encode_white cs
encode_white (c:cs) = c : encode_white cs
encode_white [] = []

decode_white :: String -> FilePath
decode_white ('\\':cs) =
    case break (=='\\') cs of
    (theord, '\\':rest) ->
        chr (read theord) : decode_white rest
    _ -> error "malformed filename"
decode_white (c:cs) = c: decode_white cs
decode_white "" = ""

own_name :: FileName -> FileName
own_name (FN f) = case breakLast '/' f of Nothing -> FN f
                                          Just (_,f') -> FN f'
super_name :: FileName -> FileName
super_name fn = case norm_path fn of
                FN f -> case breakLast '/' f of
                        Nothing -> FN "."
                        Just (d,_) -> FN d
break_on_dir :: FileName -> Maybe (FileName,FileName)
break_on_dir (FN p) = case breakFirst '/' p of
                      Nothing -> Nothing
                      Just (d,f) | d == "." -> break_on_dir $ FN f
                                 | otherwise -> Just (FN d, FN f)
norm_path :: FileName -> FileName -- remove "./"
norm_path (FN p) = FN $ repath $ drop_dotdot $ breakup p

repath :: [String] -> String
repath [] = ""
repath [f] = f
repath (d:p) = d ++ "/" ++ repath p

drop_dotdot :: [String] -> [String]
drop_dotdot ("":p) = drop_dotdot p
drop_dotdot (".":p) = drop_dotdot p
drop_dotdot ("..":p) = ".." : (drop_dotdot p)
drop_dotdot (_:"..":p) = drop_dotdot p
drop_dotdot (d:p) = case drop_dotdot p of
                    ("..":p') -> p'
                    p' -> d : p'
drop_dotdot [] = []

-- | Split a file path at the slashes
breakup :: String -> [String]
breakup p = case break (=='/') p of
            (d,"") -> [d]
            (d,p') -> d : breakup (tail p')

breakFirst :: Char -> String -> Maybe (String,String)
breakFirst c l = bf [] l
    where bf a (r:rs) | r == c = Just (reverse a,rs)
                      | otherwise = bf (r:a) rs
          bf _ [] = Nothing
breakLast :: Char -> String -> Maybe (String,String)
breakLast c l = case breakFirst c (reverse l) of
                Nothing -> Nothing
                Just (a,b) -> Just (reverse b, reverse a)

(///) :: FileName -> FileName -> FileName
(FN "")///b = norm_path b
a///b = norm_path $ fp2fn $ fn2fp a ++ "/" ++ fn2fp b

movedirfilename :: FileName -> FileName -> FileName -> FileName
movedirfilename old new name =
    if name' == old' then new
                     else if length name' > length old' &&
                             take (length old'+1) name' == old'++"/"
                          then if take 2 (fn2fp old) == "./"
                               then fp2fn ("./"++new'++drop (length old') name')
                               else fp2fn (new'++drop (length old') name')
                          else name
    where old' = fn2fp $ norm_path old
          new' = fn2fp $ norm_path new
          name' = fn2fp $ norm_path name
