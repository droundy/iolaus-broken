{-# LANGUAGE BangPatterns, ForeignFunctionInterface, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Iolaus.ByteStringUtils
-- Copyright   :  (c) The University of Glasgow 2001,
--                    David Roundy 2003-2005,2009
-- License : GPL (I'm happy to also license this file BSD style but don't
--           want to bother distributing two license files with iolaus.
--
-- Maintainer  :  droundy@abridgegame.org
-- Stability   :  experimental
-- Portability :  portable
--
-- GZIp and MMap IO for ByteStrings, and miscellaneous functions for Data.ByteString
--

module Iolaus.ByteStringUtils ( linesPS, unlinesPS, breakFirstPS,
                                breakLastPS ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-- TODO: replace breakFirstPS and breakLastPS with definitions based on
-- ByteString's break/breakEnd
{-# INLINE breakFirstPS #-}
breakFirstPS :: Char -> B.ByteString -> Maybe (B.ByteString,B.ByteString)
breakFirstPS c p = case BC.elemIndex c p of
                   Nothing -> Nothing
                   Just n -> Just (B.take n p, B.drop (n+1) p)

{-# INLINE breakLastPS #-}
breakLastPS :: Char -> B.ByteString -> Maybe (B.ByteString,B.ByteString)
breakLastPS c p = case BC.elemIndexEnd c p of
                  Nothing -> Nothing
                  Just n -> Just (B.take n p, B.drop (n+1) p)

-- TODO: rename
{-# INLINE linesPS #-}
linesPS :: B.ByteString -> [B.ByteString]
linesPS ps
     | B.null ps = [B.empty]
     | otherwise = BC.split '\n' ps

{- QuickCheck property:

import Test.QuickCheck
import qualified Data.ByteString.Char8 as BC
import Data.Char
instance Arbitrary BC.ByteString where
    arbitrary = fmap BC.pack arbitrary
instance Arbitrary Char where
  arbitrary = chr `fmap` choose (32,127)
deepCheck = check (defaultConfig { configMaxTest = 10000})
testLines =  deepCheck (\x -> (linesPS x == linesPSOld x))
linesPSOld ps = case  BC.elemIndex '\n' ps of
             Nothing -> [ps]
             Just n -> B.take n ps : linesPS (B.drop (n+1) ps) -}

{-| This function acts exactly like the "Prelude" unlines function, or like
"Data.ByteString.Char8" 'unlines', but with one important difference: it will
produce a string which may not end with a newline! That is:

> unlinesPS ["foo", "bar"]

evaluates to \"foo\\nbar\", not \"foo\\nbar\\n\"! This point should hold true for
'linesPS' as well.

TODO: rename this function. -}
unlinesPS :: [B.ByteString] -> B.ByteString
unlinesPS [] = BC.empty
unlinesPS x  = BC.init $ BC.unlines x
{-# INLINE unlinesPS #-}
{- QuickCheck property:

testUnlines = deepCheck (\x -> (unlinesPS x == unlinesPSOld x))
unlinesPSOld ss = BC.concat $ intersperse_newlines ss
    where intersperse_newlines (a:b:s) = a : newline : intersperse_newlines (b:s)
          intersperse_newlines s = s
          newline = BC.pack "\n" -}
