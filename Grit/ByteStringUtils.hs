{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE BangPatterns, ForeignFunctionInterface, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Grit.ByteStringUtils
-- Copyright   :  (c) The University of Glasgow 2001,
--                    David Roundy 2003-2005
-- License : GPL (I'm happy to also license this file BSD style but don't
--           want to bother distributing two license files with darcs.
--
-- Maintainer  :  droundy@abridgegame.org
-- Stability   :  experimental
-- Portability :  portable
--
-- GZIp and MMap IO for ByteStrings, and miscellaneous functions for Data.ByteString
--

module Grit.ByteStringUtils (

        unsafeWithInternals,

        -- list utilities
        ifHeadThenTail,
        dropSpace,
        breakSpace,
        linesPS,
        unlinesPS,
        hashPS,
        breakFirstPS,
        breakLastPS,
        substrPS,
        readIntPS,
        betweenLinesPS,
        break_after_nth_newline,
        break_before_nth_newline,
        intercalate
    ) where

-- import Autoconf                 ( use_mmap )

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
#if __GLASGOW_HASKELL__ > 606
import qualified Data.ByteString.Internal   as BI
import Data.ByteString (intercalate, uncons)
import Data.ByteString.Internal (fromForeignPtr)
#else
import qualified Data.ByteString.Base     as BI
#endif

import System.IO
import System.IO.Unsafe         ( unsafePerformIO )

import Foreign.Storable         ( peekElemOff, peek )
import Foreign.Marshal.Array    ( advancePtr )

import Data.Bits                ( rotateL )
import Data.Char                ( isSpace )
import Data.Word                ( Word8 )
import Data.Int                 ( Int32 )

import Foreign.Ptr ( plusPtr, Ptr )
import Foreign.ForeignPtr       ( withForeignPtr )

-- -----------------------------------------------------------------------------
-- unsafeWithInternals

-- | Do something with the internals of a PackedString. Beware of
-- altering the contents!
unsafeWithInternals :: B.ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
unsafeWithInternals ps f
 = case BI.toForeignPtr ps of
   (fp,s,l) -> withForeignPtr fp $ \p -> f (p `plusPtr` s) l

-- | readIntPS skips any whitespace at the beginning of its argument, and
-- reads an Int from the beginning of the PackedString.  If there is no
-- integer at the beginning of the string, it returns Nothing, otherwise it
-- just returns the int read, along with a B.ByteString containing the
-- remainder of its input.

readIntPS :: B.ByteString -> Maybe (Int, B.ByteString)
readIntPS = BC.readInt . BC.dropWhile isSpace

-- -----------------------------------------------------------------------------
-- List-mimicking functions for PackedStrings

{-# INLINE ifHeadThenTail #-}
ifHeadThenTail :: Word8 -> B.ByteString -> Maybe B.ByteString
ifHeadThenTail c s = case uncons s of
    Just (w, t) | w == c    -> Just t
    _                       -> Nothing

#if __GLASGOW_HASKELL__ <= 606
-- | /O(1)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: B.ByteString -> Maybe (Word8, B.ByteString)
uncons (BI.PS x s l)
    | l <= 0    = Nothing
    | otherwise = Just (BI.inlinePerformIO $ withForeignPtr x
                                        $ \p -> peekByteOff p s,
                        BI.PS x (s+1) (l-1))
{-# INLINE uncons #-}
-- | /O(1)/ Build a ByteString from a ForeignPtr
fromForeignPtr :: ForeignPtr Word8
               -> Int -- ^ Offset
               -> Int -- ^ Length
               -> B.ByteString
fromForeignPtr fp s l = BI.PS fp s l
{-# INLINE fromForeignPtr #-}
-- | /O(n)/ The 'intercalate' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: B.ByteString -> [B.ByteString] -> B.ByteString
intercalate s = B.concat . (intersperse s)
{-# INLINE [1] intercalate #-}
#endif


------------------------------------------------------------------------
-- A reimplementation of Data.ByteString.Char8.dropSpace, but
-- specialised to darcs' need for a 4 way isspace.
--
-- TODO: if it is safe to use the expanded definition of isSpaceWord8
-- provided by Data.ByteString.Char8, then all this can go.

-- A locale-independent isspace(3) so patches are interpreted the same everywhere.
-- ((c) == ' ' || (c) == '\t' || (c) == '\n' || (c) == '\r')
isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 w =
    w == 0x20 ||    -- ' '
    w == 0x09 ||    -- '\t'
    w == 0x0A ||    -- '\n'
    w == 0x0D       -- '\r'
{-# INLINE isSpaceWord8 #-}

firstnonspace :: Ptr Word8 -> Int -> Int -> IO Int
firstnonspace !ptr !n !m
    | n >= m    = return n
    | otherwise = do w <- peekElemOff ptr n
                     if isSpaceWord8 w then firstnonspace ptr (n+1) m else return n

firstspace :: Ptr Word8 -> Int -> Int -> IO Int
firstspace !ptr !n !m
    | n >= m    = return n
    | otherwise = do w <- peekElemOff ptr n
                     if (not . isSpaceWord8) w then firstspace ptr (n+1) m else return n

-- | 'dropSpace' efficiently returns the 'ByteString' argument with
-- white space Chars removed from the front. It is more efficient than
-- calling dropWhile for removing whitespace. I.e.
-- 
-- > dropWhile isSpace == dropSpace
--
dropSpace :: B.ByteString -> B.ByteString
dropSpace (BI.PS x s l) = BI.inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- firstnonspace (p `plusPtr` s) 0 l
    return $! if i == l then B.empty else BI.PS x (s+i) (l-i)
{-# INLINE dropSpace #-}

-- | 'breakSpace' returns the pair of ByteStrings when the argument is
-- broken at the first whitespace byte. I.e.
-- 
-- > break isSpace == breakSpace
--
breakSpace :: B.ByteString -> (B.ByteString,B.ByteString)
breakSpace (BI.PS x s l) = BI.inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- firstspace (p `plusPtr` s) 0 l
    return $! case () of {_
        | i == 0    -> (B.empty, BI.PS x s l)
        | i == l    -> (BI.PS x s l, B.empty)
        | otherwise -> (BI.PS x s i, BI.PS x (s+i) (l-i))
    }
{-# INLINE breakSpace #-}

------------------------------------------------------------------------

-- ByteString rewrites break (=='x') to breakByte 'x'
--  break ((==) x) = breakChar x
--  break (==x) = breakChar x
--

{-
{-# INLINE breakOnPS #-}
breakOnPS :: Char -> B.ByteString -> (B.ByteString, B.ByteString)
breakOnPS c p = case BC.elemIndex c p of
                Nothing -> (p, BC.empty)
                Just n  -> (B.take n p, B.drop n p)
-}

{-# INLINE hashPS #-}
hashPS :: B.ByteString -> Int32
hashPS ps =
   case BI.toForeignPtr ps of
   (x,s,l) ->
    unsafePerformIO $ withForeignPtr x $ \p->
    do hash (p `plusPtr` s) l

hash :: Ptr Word8 -> Int -> IO Int32
hash ptr len = f (0 :: Int32) ptr len
 where f h _ 0 = return h
       f h p n = do x <- peek p
                    let !h' =  (fromIntegral x) + (rotateL h 8)
                    f h' (p `advancePtr` 1) (n-1)

{-# INLINE substrPS #-}
substrPS :: B.ByteString -> B.ByteString -> Maybe Int
substrPS tok str
    | B.null tok = Just 0
    | B.length tok > B.length str = Nothing
    | otherwise = do n <- BC.elemIndex (BC.head tok) str
                     let ttok = B.tail tok
                         reststr = B.drop (n+1) str
                     if ttok == B.take (B.length ttok) reststr
                        then Just n
                        else ((n+1)+) `fmap` substrPS tok reststr

------------------------------------------------------------------------

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

-- -------------------------------------------------------------------------
-- betweenLinesPS

-- | betweenLinesPS returns the B.ByteString between the two lines given,
-- or Nothing if they do not appear.

betweenLinesPS :: B.ByteString -> B.ByteString -> B.ByteString
               -> Maybe (B.ByteString)
betweenLinesPS start end ps
 = case break (start ==) (linesPS ps) of
       (_, _:rest@(bs1:_)) ->
           case BI.toForeignPtr bs1 of
            (ps1,s1,_) ->
             case break (end ==) rest of
               (_, bs2:_) -> case BI.toForeignPtr bs2 of (_,s2,_) -> Just $ fromForeignPtr ps1 s1 (s2 - s1)
               _ -> Nothing
       _ -> Nothing

-- -------------------------------------------------------------------------
-- break_after_nth_newline

break_after_nth_newline :: Int -> B.ByteString
                        -> Maybe (B.ByteString, B.ByteString)
break_after_nth_newline 0 the_ps | B.null the_ps = Just (B.empty, B.empty)
break_after_nth_newline n the_ps =
  case BI.toForeignPtr the_ps of
  (fp,the_s,l) ->
   unsafePerformIO $ withForeignPtr fp $ \p ->
   do let findit 0 s | s == end = return $ Just (the_ps, B.empty)
          findit _ s | s == end = return Nothing
          findit 0 s = let left_l = s - the_s
                       in return $ Just (fromForeignPtr fp the_s left_l,
                                         fromForeignPtr fp s (l - left_l))
          findit i s = do w <- peekElemOff p s
                          if w == nl then findit (i-1) (s+1)
                                     else findit i (s+1)
          nl = BI.c2w '\n'
          end = the_s + l
      findit n the_s

-- -------------------------------------------------------------------------
-- break_before_nth_newline

break_before_nth_newline :: Int -> B.ByteString -> (B.ByteString, B.ByteString)
break_before_nth_newline 0 the_ps
 | B.null the_ps = (B.empty, B.empty)
break_before_nth_newline n the_ps =
 case BI.toForeignPtr the_ps of
 (fp,the_s,l) ->
   unsafePerformIO $ withForeignPtr fp $ \p ->
   do let findit _ s | s == end = return (the_ps, B.empty)
          findit i s = do w <- peekElemOff p s
                          if w == nl
                            then if i == 0
                                 then let left_l = s - the_s
                                      in return (fromForeignPtr fp the_s left_l,
                                                 fromForeignPtr fp s (l - left_l))
                                 else findit (i-1) (s+1)
                            else findit i (s+1)
          nl = BI.c2w '\n'
          end = the_s + l
      findit n the_s
