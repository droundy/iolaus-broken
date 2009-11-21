-- Copyright (C) 2003 David Roundy
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

{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Iolaus.Lock ( withTempDir, withPermDir, appendToFile,
                     writeBinFile, readBinFile, writeAtomicFilePS,
                     removeFileMayNotExist,
                     world_readable_temp ) where

#ifdef WIN32
import qualified System.Directory ( renameFile )
#endif

import Prelude hiding ( catch )
import Data.Maybe ( isJust )
import System.IO ( openBinaryFile, hClose, hPutStr, Handle,
                   IOMode(WriteMode, AppendMode) )
import System.IO.Error ( isDoesNotExistError, isAlreadyExistsError )
import Control.Exception ( bracket, catchJust, ioErrors, throwIO,
                           Exception(IOException), catch )
import System.Directory ( removeFile, removeDirectory,
                   doesFileExist, doesDirectoryExist,
                   getDirectoryContents, createDirectory,
                   getTemporaryDirectory, renameFile )
import Iolaus.Utils ( withCurrentDirectory, maybeGetEnv, firstJustIO )
import Control.Monad ( unless, when )

import Iolaus.Utils ( catchall, add_to_error_loc )
import Iolaus.RepoPath ( AbsolutePath, FilePathLike, toFilePath,
                         makeAbsolute, ioAbsolute, (<++>),
                         getCurrentDirectory, setCurrentDirectory )

import qualified Data.ByteString as B (readFile, hPut, ByteString)
import qualified Data.ByteString.Char8 as BC (unpack)

import Iolaus.SignalHandler ( withSignalsBlocked )
import Iolaus.Global ( atexit )
import System.Posix.Files ( getSymbolicLinkStatus, isDirectory, stdFileMode,
                            fileMode, getFileStatus, setFileMode )
import System.Posix.IO ( openFd, closeFd, defaultFileFlags, exclusive,
                         OpenMode(WriteOnly) )
#include "impossible.h"

removeFileMayNotExist :: FilePathLike p => p -> IO ()
removeFileMayNotExist f = catchNonExistence (removeFile $ toFilePath f) ()

catchNonExistence :: IO a -> a -> IO a
catchNonExistence job nonexistval =
    catchJust ioErrors job $
    \e -> if isDoesNotExistError e then return nonexistval
                                   else ioError e

takeFile :: FilePath -> IO Bool
takeFile fp =
    do sloppy_atomic_create fp
       return True
  `catch` \e -> case e of
                    IOException e'
                     | isAlreadyExistsError e' ->
                        return False
                    _ -> do pwd <- getCurrentDirectory
                            throwIO $ add_to_error_loc e
                                            ("takeFile "++fp++" in "++toFilePath pwd)

tempdir_loc :: IO AbsolutePath
tempdir_loc =
    firstJustIO [ maybeGetEnv "IOLAUS_TMPDIR" >>= chkdir,
                  maybeGetEnv "DARCS_TMPDIR" >>= chkdir,
                  getTemporaryDirectory >>= chkdir . Just,
                  Just `fmap` getCurrentDirectory ]
                    >>= return . fromJust
    where chkdir Nothing = return Nothing
          chkdir (Just d) =
              do exists <- doesDirectoryExist d
                 if exists then Just `fmap` ioAbsolute d
                           else return Nothing

data WithDirKind = Perm | Temp

withDir :: WithDirKind -> String -> (AbsolutePath -> IO a) -> IO a
withDir kind abs_or_relative_name job = do
  absolute_name <- (`makeAbsolute` abs_or_relative_name) `fmap` tempdir_loc
  formerdir <- getCurrentDirectory
  bracket (create_directory absolute_name 0)
          (\dir -> do setCurrentDirectory formerdir
                      k <- keep_tmpdir
                      unless k $ do case kind of
                                      Perm -> return ()
                                      Temp -> rm_recursive (toFilePath dir))
          job
    where newname name 0 = name
          newname name n = name <++> ('-':show n)
          create_directory :: AbsolutePath -> Int -> IO AbsolutePath
          create_directory name n
              = do createDirectory $ toFilePath $ newname name n
                   setCurrentDirectory $ newname name n
                   getCurrentDirectory
                `catch` (\e -> case e of
                              IOException e'
                               | isAlreadyExistsError e' ->
                                  create_directory name (n+1)
                              _ -> throwIO e)
          keep_tmpdir = isJust `fmap` maybeGetEnv "DARCS_KEEP_TMPDIR"

-- |'withPermDir' is like 'withTempDir', except that it doesn't
-- delete the directory afterwards.
withPermDir :: String -> (AbsolutePath -> IO a) -> IO a
withPermDir = withDir Perm

-- |'withTempDir' creates an empty directory and then removes it when it
-- is no longer needed.  withTempDir creates a temporary directory.  The
-- location of that directory is determined by the contents of
-- @$IOLAUS_TMPDIR, if it exists, otherwise by @$DARCS_TMPDIR@, and if
-- that doesn't exist then whatever your operating system considers to be a
-- a temporary directory (e.g. @$TMPDIR@ under Unix, @$TEMP@ under
-- Windows).
withTempDir :: String -> (AbsolutePath -> IO a) -> IO a
withTempDir = withDir Temp

doesDirectoryReallyExist :: FilePath -> IO Bool
doesDirectoryReallyExist f =
    catchNonExistence (isDirectory `fmap` getSymbolicLinkStatus f) False

rm_recursive :: FilePath -> IO ()
rm_recursive d =
    do isd <- doesDirectoryReallyExist d
       if not isd
          then removeFile d
          else when isd $ do conts <- actual_dir_contents
                             withCurrentDirectory d $
                               mapM_ rm_recursive conts
                             removeDirectory d
    where actual_dir_contents = -- doesn't include . or ..
              do c <- getDirectoryContents d
                 return $ filter (/=".") $ filter (/="..") c

world_readable_temp :: String -> IO String
world_readable_temp f = wrt 0
    where wrt :: Int -> IO String
          wrt 100 = fail $ "Failure creating temp named "++f
          wrt n = let f_new = f++"-"++show n
                  in do ok <- takeFile f_new
                        if ok then do atexit $ removeFileMayNotExist f_new
                                      return f_new
                              else wrt (n+1)

withNamedTemp :: String -> (String -> IO a) -> IO a
withNamedTemp n = bracket get_empty_file removeFileMayNotExist
    where get_empty_file = world_readable_temp n

readFilePS :: FilePathLike p => p -> IO B.ByteString
readFilePS = B.readFile . toFilePath

readBinFile :: FilePathLike p => p -> IO String
readBinFile = fmap BC.unpack . readFilePS

writeBinFile :: FilePathLike p => p -> String -> IO ()
writeBinFile f s = writeToFile f $ \h -> hPutStr h s

writeAtomicFilePS :: FilePathLike p => p -> B.ByteString -> IO ()
writeAtomicFilePS f ps = writeToFile f $ \h -> B.hPut h ps

writeToFile :: FilePathLike p => p -> (Handle -> IO ()) -> IO ()
writeToFile f job =
    withSignalsBlocked $ withNamedTemp (toFilePath f) $ \newf -> do
    bracket (openBinaryFile newf WriteMode) hClose job
    already_exists <- doesFileExist (toFilePath f)
    when already_exists $ do mode <- fileMode `fmap` getFileStatus (toFilePath f)
                             setFileMode newf mode
             `catchall` return ()
    renameFile newf (toFilePath f)

appendToFile :: FilePathLike p => p -> (Handle -> IO ()) -> IO ()
appendToFile f job = withSignalsBlocked $ 
    bracket (openBinaryFile (toFilePath f) AppendMode) hClose job

sloppy_atomic_create :: FilePath -> IO ()
sloppy_atomic_create fp
    = do fd <- openFd fp WriteOnly (Just stdFileMode) flags
         closeFd fd
  where flags = defaultFileFlags { exclusive = True }

#ifdef WIN32
{- System.Directory.renameFile incorrectly fails when the new file already
   exists.  This code works around that bug at the cost of losing atomic
   writes. -}

renameFile :: FilePath -> FilePath -> IO ()
renameFile old new = Control.Exception.block $
   System.Directory.renameFile old new
   `System.IO.Error.catch` \_ ->
   do System.Directory.removeFile new
        `System.IO.Error.catch`
         (\e -> if System.IO.Error.isDoesNotExistError e
                   then return ()
                   else System.IO.Error.ioError e)
      System.Directory.renameFile old new
#endif
