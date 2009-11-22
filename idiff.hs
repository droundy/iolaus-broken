-- Copyright (C) 2009 David Roundy
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

module Main (main) where

import System.IO ( hSetBinaryMode)
import System.IO ( stdin, stdout )
import System.Environment ( getArgs )
import Control.Exception ( Exception( AssertionFailed ), handleJust )

import qualified Data.ByteString.Char8 as BC ( pack )
import qualified Data.ByteString as B ( readFile )

import Iolaus.Patch ( chunkify, chunk, showContextPatch, canonize )
import Iolaus.SlurpDirectory ( slurp )
import Iolaus.Printer ( putDocLn )
import Iolaus.SignalHandler ( withSignalsHandled )
import Iolaus.Global ( with_atexit )
#include "impossible.h"

assertions :: Control.Exception.Exception -> Maybe String
assertions (AssertionFailed s) = Just s
assertions _ = Nothing

main :: IO ()
main = with_atexit $ withSignalsHandled $
  handleJust assertions bug $ do
  argv <- getArgs
  case argv of [a,b] -> idiff a b
               _ -> fail "give two files"

idiff :: FilePath -> FilePath -> IO ()
idiff a b =
    do hSetBinaryMode stdin True
       hSetBinaryMode stdout True
       o <- chunkify newlines `fmap` B.readFile a
       n <- chunkify newlines `fmap` B.readFile b
       let ch = canonize $ chunk a newlines 0 o n
       s <- slurp a
       putDocLn $ showContextPatch s ch
    where newlines = BC.pack " ,\n()"
