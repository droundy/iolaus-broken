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
module Main where

import Data.Maybe ( isNothing )
import System.Environment ( getArgs )

import Git.Plumbing ( updateIndexForceRemove, updateIndexCacheInfo,
                      Blob(Blob), Hash, mkHash,
                      unpackFile, mergeFile )
#include "gadts.h"
#include "impossible.h"

main :: IO ()
main = do [sa,s1,s2,fp,ma,m1,m2] <- getArgs
          mergeit (hashit sa) (hashit s1) (hashit s2) fp ma m1 m2

hashit :: String -> Maybe (Hash Blob C(x))
hashit "" = Nothing
hashit x = Just $ mkHash Blob x

mergeit :: Maybe (Hash Blob C(x)) -> Maybe (Hash Blob C(x))
        -> Maybe (Hash Blob C(x))
        -> FilePath
        -> String -> String -> String -> IO ()
mergeit sa s1 s2 fp _ _ _ -- deleted in one or both
    | (isNothing s1 && s2 == sa) ||
      (isNothing s2 && s1 == sa) ||
      (isNothing s1 && isNothing s2) =
            do putStrLn $ "Removing "++fp
               updateIndexForceRemove fp
mergeit Nothing s1 s2 fp _ m1 m2 -- added in just one
    | isNothing s1 || isNothing s2 || s1 == s2 =
      do (m,sh) <- case (s1,s2) of
                     (Nothing, Just h) -> return (m2,h)
                     (Just h, Nothing) -> return (m1,h)
                     (Just a, Just b)
                         | a == b -> return (mergemodes m1 m2, a)
                         | otherwise -> impossible
                     (Nothing, Nothing) -> impossible
         putStrLn $ "Added "++fp
         updateIndexCacheInfo m sh fp
mergeit msa (Just s1) (Just s2) fp ma m1 m2 =
    do f1 <- unpackFile s1
       f2 <- unpackFile s2
       fa <- case msa of Just sa -> unpackFile sa
                         Nothing -> do x <- unpackFile s1
                                       writeFile x ""
                                       return x
       let m = if ma == m1
               then m2
               else if ma == m2
                    then m1
                    else if '7' `elem` (m1++m2)
                         then "100755"
                         else m1
       out <- mergeFile f1 fa f2
       updateIndexCacheInfo m out fp
mergeit _ _ _ _ _ _ _ = impossible

mergemodes :: String -> String -> String
mergemodes "" m = m
mergemodes m "" = m
mergemodes m1 m2 | '7' `elem` (m1++m2) = "100755"
mergemodes m _ = m -- just pick one...
