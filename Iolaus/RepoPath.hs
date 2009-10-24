{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}

-- Copyright (C) 2007 Eric Kow
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

module Iolaus.RepoPath ( AbsolutePath, makeAbsolute, ioAbsolute, rootDirectory,
                         SubPath, makeSubPathOf, simpleSubPath,
                         AbsolutePathOrStd,
                         makeAbsoluteOrStd, ioAbsoluteOrStd, useAbsoluteOrStd,
                         FilePathOrURL(toPath, (</>), (<++>)),
                         FilePathLike(toFilePath, parentDir),
                         SubPathLike,
                         doesDirectoryExist, doesFileExist,
                         createDirectory, createDirectoryIfMissing,
                         getCurrentDirectory, setCurrentDirectory
                       ) where

import Data.List ( isPrefixOf )
import Control.Exception ( bracket )

import Iolaus.Progress ( debugMessage )
import qualified Iolaus.Workaround ( getCurrentDirectory )
import qualified System.Directory ( setCurrentDirectory,
                                    createDirectory, createDirectoryIfMissing,
                                    doesDirectoryExist, doesFileExist )
import qualified System.FilePath.Posix as FilePath ( normalise )
import qualified System.FilePath as NativeFilePath ( takeFileName, takeDirectory )
import qualified Iolaus.FileName as PatchFileName ( FileName, super_name, fp2fn, fn2fp,
                                                   (///) )

class Eq a => FilePathOrURL a where
 {-# INLINE toPath #-}
 toPath :: a -> String
 (</>) :: SubPathLike b => a -> b -> a
 (<++>) :: a -> String -> a -- ^ append string to filename

class FilePathOrURL a => FilePathLike a where
 {-# INLINE toFilePath #-}
 toFilePath :: a -> FilePath
 parentDir :: a -> a

class SubPathSecret a where
class (SubPathSecret a, FilePathLike a) => SubPathLike a where

-- | Relative to the local darcs repository and normalized
--   Note: these are understood not to have the dot in front
newtype SubPath      = SubPath FilePath deriving (Eq, Ord)
newtype AbsolutePath = AbsolutePath FilePath deriving (Eq, Ord)
data AbsolutePathOrStd = AP AbsolutePath | APStd deriving (Eq, Ord)

instance FilePathOrURL AbsolutePath where
 toPath (AbsolutePath x) = x
 AbsolutePath x </> sp = AbsolutePath (x </> toFilePath sp)
 AbsolutePath x <++> y = AbsolutePath (x++y)
instance FilePathOrURL SubPath where
 toPath (SubPath x) = x
 SubPath x </> sp = SubPath (x </> toFilePath sp)
 SubPath x <++> y = SubPath (x++y)
instance CharLike c => FilePathOrURL [c] where
 toPath = toFilePath
 a </> sp = cleana ++ fromChar '/' : dropWhile sl (map fromChar $ toFilePath sp)
     where sl = (== '/') . toChar
           cleana = case reverse (dropWhile sl $ reverse a) of
                      [] -> a
                      _:[c] | toChar c == ':' -> a
                      a' -> a'
 x <++> y = x ++ map fromChar y

instance SubPathSecret SubPath
instance SubPathLike SubPath
instance CharLike c => SubPathSecret [c]
instance CharLike c => SubPathLike [c]
instance SubPathSecret PatchFileName.FileName
instance SubPathLike PatchFileName.FileName

instance FilePathOrURL PatchFileName.FileName where
    toPath p = case PatchFileName.fn2fp p of -- cut annoying ./ from paths
                 "./" -> "."
                 '.':'/':p' -> p'
                 p' -> p'
    fn </> sp = fn PatchFileName./// (PatchFileName.fp2fn $ toFilePath sp)
    x <++> y = PatchFileName.fp2fn $ toFilePath x ++ y
instance FilePathLike PatchFileName.FileName where
    toFilePath = toPath
    parentDir = PatchFileName.super_name

instance FilePathLike AbsolutePath where
 toFilePath (AbsolutePath x) = x
 parentDir (AbsolutePath x) = AbsolutePath (parentDir x)
instance FilePathLike SubPath where
 toFilePath (SubPath x) = x
 parentDir (SubPath x) = SubPath (parentDir x)

class Eq c => CharLike c where
    toChar :: c -> Char
    fromChar :: Char -> c
instance CharLike Char where
    toChar = id
    fromChar = id

instance CharLike c => FilePathLike [c] where
    toFilePath = map toChar
    parentDir xs = case dropWhile (/= '/') $ reverse $ map toChar xs of
                     [] -> [fromChar '.']
                     xs' -> case dropWhile (== '/') xs' of
                              [_,':'] -> map fromChar $ reverse xs' -- drive letter 
                              [] -> map fromChar $ reverse xs' -- root directory
                              xs'' -> map fromChar $ reverse xs''

-- | Make the second path relative to the first, if possible
makeSubPathOf :: AbsolutePath -> AbsolutePath -> Maybe SubPath
makeSubPathOf (AbsolutePath p1) (AbsolutePath p2) =
 -- The slash prevents "foobar" from being treated as relative to "foo"
 if p1 == p2 || (p1 ++ "/") `isPrefixOf` p2
    then Just $ SubPath $ drop (length p1 + 1) p2
    else Nothing

simpleSubPath :: FilePath -> Maybe SubPath
simpleSubPath x | is_relative x = Just $ SubPath $ FilePath.normalise $ map cleanup x
                | otherwise = Nothing

-- | Interpret a possibly relative path wrt the current working directory
ioAbsolute :: FilePath -> IO AbsolutePath
ioAbsolute dir =
    do isdir <- doesDirectoryExist dir
       here <- getCurrentDirectory
       if isdir
         then bracket (setCurrentDirectory dir)
                      (const $ setCurrentDirectory $ toFilePath here)
                      (const getCurrentDirectory)
         else let super_dir = case NativeFilePath.takeDirectory dir of
                                "" ->  "."
                                d  -> d
                  file = NativeFilePath.takeFileName dir
              in do abs_dir <- if dir == super_dir
                               then return $ AbsolutePath dir
                               else ioAbsolute super_dir
                    return $ makeAbsolute abs_dir file

makeAbsolute :: AbsolutePath -> FilePath -> AbsolutePath
makeAbsolute a dir = if is_absolute dir
                     then AbsolutePath $
                          slashes ++ FilePath.normalise cleandir
                     else ma a $ FilePath.normalise cleandir
  where
    cleandir  = map cleanup dir
    slashes = norm_slashes $ takeWhile (== '/') cleandir
    ma here ('.':'.':'/':r) = ma (parentDir here) r
    ma here ".." = parentDir here
    ma here "." = here
    ma here "" = here
    ma here r = here /- r

(/-) :: AbsolutePath -> String -> AbsolutePath
x /- ('/':r) = x /- r
(AbsolutePath "/") /- r = AbsolutePath ('/':simpleClean r)
(AbsolutePath x) /- r = AbsolutePath (x++'/':simpleClean r)

simpleClean :: String -> String
simpleClean x = norm_slashes $ reverse $ dropWhile (=='/') $ reverse $
                map cleanup x

rootDirectory :: AbsolutePath
rootDirectory = AbsolutePath "/"

makeAbsoluteOrStd :: AbsolutePath -> String -> AbsolutePathOrStd
makeAbsoluteOrStd _ "-" = APStd
makeAbsoluteOrStd a p = AP $ makeAbsolute a p

ioAbsoluteOrStd :: String -> IO AbsolutePathOrStd
ioAbsoluteOrStd "-" = return APStd
ioAbsoluteOrStd p = AP `fmap` ioAbsolute p

useAbsoluteOrStd :: (AbsolutePath -> IO a) -> IO a -> AbsolutePathOrStd -> IO a
useAbsoluteOrStd _ f APStd = f
useAbsoluteOrStd f _ (AP x) = f x

instance Show AbsolutePath where
 show = show . toFilePath
instance Show SubPath where
 show = show . toFilePath
instance Show AbsolutePathOrStd where
    show (AP a) = show a
    show APStd = "standard input/output"

cleanup :: Char -> Char
cleanup '\\' = '/'
cleanup c = c

norm_slashes :: String -> String
#ifndef WIN32
-- multiple slashes in front are ignored under Unix
norm_slashes ('/':p) = '/' : dropWhile (== '/') p
#endif
norm_slashes p = p

getCurrentDirectory :: IO AbsolutePath
getCurrentDirectory = AbsolutePath `fmap` Iolaus.Workaround.getCurrentDirectory

setCurrentDirectory :: FilePathLike p => p -> IO ()
setCurrentDirectory = System.Directory.setCurrentDirectory . toFilePath

doesFileExist :: FilePathLike p => p -> IO Bool
doesFileExist = System.Directory.doesFileExist . toFilePath

doesDirectoryExist :: FilePathLike p => p -> IO Bool
doesDirectoryExist = System.Directory.doesDirectoryExist . toFilePath

createDirectory :: FilePathLike p => p -> IO ()
createDirectory p =
    do debugMessage $ "createDirectory "++ toFilePath p
       System.Directory.createDirectory $ toFilePath p

createDirectoryIfMissing :: FilePathLike p => Bool -> p -> IO ()
createDirectoryIfMissing b p =
    do debugMessage $ "createDirectoryIfMissing "++ show b++" "++toFilePath p
       System.Directory.createDirectoryIfMissing b $ toFilePath p

is_relative :: String -> Bool
is_relative (_:':':_) = False
is_relative (c:_) = c /= '/' && c /= '~'
is_relative "" = False

is_absolute :: String -> Bool
is_absolute = not . is_relative
