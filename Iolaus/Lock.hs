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

module Iolaus.Lock ( withTemp, withOpenTemp, withStdoutTemp,
                   withTempDir, withPermDir, withDelayedDir, withNamedTemp,
                   writeToFile, appendToFile,
                   writeBinFile, writeDocBinFile, appendBinFile, appendDocBinFile,
                   readFilePS, readBinFile, readDocBinFile,
                   writeAtomicFilePS,
                   rm_recursive, removeFileMayNotExist,
                   canonFilename,
                   world_readable_temp, tempdir_loc ) where

import Prelude hiding ( catch )
import Data.List ( inits )
import Data.Maybe ( isJust, listToMaybe )
import System.IO ( openBinaryFile, openBinaryTempFile,
                   hClose, hPutStr, Handle,
                   IOMode(WriteMode, AppendMode) )
import System.IO.Error ( isDoesNotExistError, isAlreadyExistsError )
import Control.Exception ( bracket, catchJust, ioErrors, throwIO,
                           Exception(IOException), catch, try )
import System.Directory ( removeFile, removeDirectory,
                   doesFileExist, doesDirectoryExist,
                   getDirectoryContents, createDirectory,
                   getTemporaryDirectory,
                 )
import System.FilePath.Posix ( splitDirectories )
import Iolaus.Workaround ( renameFile )
import Iolaus.Utils ( withCurrentDirectory, maybeGetEnv, firstJustIO )
import Control.Monad ( unless, when )

import Iolaus.URL ( is_relative )
import Iolaus.Utils ( catchall, add_to_error_loc )
import Iolaus.RepoPath ( AbsolutePath, FilePathLike, toFilePath,
                        getCurrentDirectory, setCurrentDirectory )

import qualified Data.ByteString as B (null, readFile, hPut, ByteString)
import qualified Data.ByteString.Char8 as BC (unpack)

import Iolaus.SignalHandler ( withSignalsBlocked )
import Iolaus.Printer ( Doc, hPutDoc, packedString, empty )
import Iolaus.Global ( atexit )
import Iolaus.Compat ( mk_stdout_temp, canonFilename,
                     sloppy_atomic_create )
import System.Posix.Files ( getSymbolicLinkStatus, isDirectory,
                            fileMode, getFileStatus, setFileMode )
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

-- |'withTemp' safely creates an empty file (not open for writing) and
-- returns its name.
--
-- The temp file operations are rather similar to the locking operations, in
-- that they both should always try to clean up, so exitWith causes trouble.
withTemp :: (String -> IO a) -> IO a
withTemp = bracket get_empty_file removeFileMayNotExist
    where get_empty_file = do (f,h) <- openBinaryTempFile "." "darcs"
                              hClose h
                              return f

-- |'withOpenTemp' creates an already open temporary
-- file.  Both of them run their argument and then delete the file.  Also,
-- both of them (to my knowledge) are not susceptible to race conditions on
-- the temporary file (as long as you never delete the temporary file; that
-- would reintroduce a race condition).
withOpenTemp :: ((Handle, String) -> IO a) -> IO a
withOpenTemp = bracket get_empty_file cleanup
    where cleanup (h,f) = do try $ hClose h
                             removeFileMayNotExist f
          get_empty_file = invert `fmap` openBinaryTempFile "." "darcs"
          invert (a,b) = (b,a)

withStdoutTemp :: (String -> IO a) -> IO a
withStdoutTemp = bracket (mk_stdout_temp "stdout_") removeFileMayNotExist

tempdir_loc :: IO FilePath
tempdir_loc = firstJustIO [ readBinFile (".arcs-prefs/tmpdir") >>= return . Just . head.words >>= chkdir,
                            maybeGetEnv "DARCS_TMPDIR" >>= chkdir,
                            getTemporaryDirectory >>= chkdir . Just,
                            getCurrentDirectorySansIolaus,
                            return $ Just "."  -- always returns a Just
                          ]
              >>= return . fromJust
    where chkdir Nothing = return Nothing
          chkdir (Just d) = doesDirectoryExist d >>= return . \e -> if e then Just (d++"/") else Nothing
                                                         
getCurrentDirectorySansIolaus :: IO (Maybe FilePath)
getCurrentDirectorySansIolaus = do
  c <- getCurrentDirectory
  return $ listToMaybe $ drop 5 $ reverse $ takeWhile no_darcs $ inits $ toFilePath c
  where no_darcs x = not $ "_darcs" `elem` splitDirectories x

data WithDirKind = Perm | Temp | Delayed

withDir :: WithDirKind -> String -> (AbsolutePath -> IO a) -> IO a
withDir kind abs_or_relative_name job = do
  absolute_name <- if is_relative abs_or_relative_name
                   then fmap (++ abs_or_relative_name) tempdir_loc
                   else return abs_or_relative_name
  formerdir <- getCurrentDirectory
  bracket (create_directory absolute_name 0)
          (\dir -> do setCurrentDirectory formerdir
                      k <- keep_tmpdir
                      unless k $ do case kind of
                                      Perm -> return ()
                                      Temp -> rm_recursive (toFilePath dir)
                                      Delayed -> atexit $ rm_recursive (toFilePath dir))
          job
    where newname name 0 = name
          newname name n = name ++ "-" ++ show n
          create_directory :: FilePath -> Int -> IO AbsolutePath
          create_directory name n
              = do createDirectory $ newname name n
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
-- .arcs-prefs/tmpdir, if it exists, otherwise by @$DARCS_TMPDIR@, and if
-- that doesn't exist then whatever your operating system considers to be a
-- a temporary directory (e.g. @$TMPDIR@ under Unix, @$TEMP@ under
-- Windows).
--
-- If none of those exist it creates the temporary directory
-- in the current directory, unless the current directory is under a _darcs
-- directory, in which case the temporary directory in the parent of the highest
-- _darcs directory to avoid accidentally corrupting darcs's internals.
-- This should not fail, but if it does indeed fail, we go ahead and use the
-- current directory anyway. If @$DARCS_KEEP_TMPDIR@ variable is set
-- temporary directory is not removed, this can be useful for debugging.
withTempDir :: String -> (AbsolutePath -> IO a) -> IO a
withTempDir = withDir Temp

withDelayedDir :: String -> (AbsolutePath -> IO a) -> IO a
withDelayedDir = withDir Delayed

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

readDocBinFile :: FilePathLike p => p -> IO Doc
readDocBinFile fp = do ps <- B.readFile $ toFilePath fp
                       return $ if B.null ps then empty else packedString ps

appendBinFile :: FilePathLike p => p -> String -> IO ()
appendBinFile f s = appendToFile f $ \h -> hPutStr h s

appendDocBinFile :: FilePathLike p => p -> Doc -> IO ()
appendDocBinFile f d = appendToFile f $ \h -> hPutDoc h d

writeBinFile :: FilePathLike p => p -> String -> IO ()
writeBinFile f s = writeToFile f $ \h -> hPutStr h s

writeDocBinFile :: FilePathLike p => p -> Doc -> IO ()
writeDocBinFile f d = writeToFile f $ \h -> hPutDoc h d

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
