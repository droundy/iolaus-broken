-- Copyright (C) 2005 David Roundy
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
module Iolaus.IO ( WriteableDirectory(..), ExecutableBit(..),
                   runTolerantly ) where

import Prelude hiding ( catch )
import Data.List ( isSuffixOf )
import System.IO.Error ( isDoesNotExistError, isPermissionError )
import Control.Exception ( catch, catchJust, ioErrors )
import System.Directory ( createDirectory, removeDirectory, removeFile,
                          renameFile, renameDirectory,
                          doesDirectoryExist, doesFileExist )
#ifndef WIN32
import System.Posix.Files (fileMode,getFileStatus, setFileMode,
                           setFileCreationMask,
                           ownerReadMode, ownerWriteMode, ownerExecuteMode,
                           groupReadMode, groupWriteMode, groupExecuteMode,
                           otherReadMode, otherWriteMode, otherExecuteMode)
import Data.Bits ( (.&.), (.|.), complement )
#endif

import qualified Data.ByteString as B (ByteString, empty, null, readFile)

import Iolaus.Show ( pretty )
import Iolaus.FileName ( FileName, fn2fp )
import Iolaus.Lock ( writeAtomicFilePS )

data ExecutableBit = IsExecutable | NotExecutable
                   deriving ( Eq, Ord )

class (Functor m, Monad m) => WriteableDirectory m where
    mDoesDirectoryExist :: FileName -> m Bool
    mDoesFileExist :: FileName -> m Bool
    mReadFilePS :: FileName -> m B.ByteString
    mSetFileExecutable :: FileName -> ExecutableBit -> m ()
    mWriteFilePS :: FileName -> B.ByteString -> m ()
    mCreateDirectory :: FileName -> m ()
    mRemoveDirectory :: FileName -> m ()
    mCreateFile :: FileName -> m ()
    mCreateFile f = mWriteFilePS f B.empty
    mRemoveFile :: FileName -> m ()
    mRename :: FileName -> FileName -> m ()
    mModifyFilePS :: FileName -> (B.ByteString -> m B.ByteString) -> m ()
    mModifyFilePS f j = do ps <- mReadFilePS f
                           ps' <- j ps
                           mWriteFilePS f ps'

instance WriteableDirectory IO where
    mDoesDirectoryExist = doesDirectoryExist . fn2fp
    mDoesFileExist = doesFileExist . fn2fp
    mReadFilePS = B.readFile . fn2fp
    mSetFileExecutable f ex =
#ifdef WIN32
        return ()
#else
        do st <- getFileStatus $ fn2fp f
           umask <- setFileCreationMask 0
           setFileCreationMask umask
           let rw = fileMode st .&.
                    (ownerReadMode .|. ownerWriteMode .|.
                     groupReadMode .|. groupWriteMode .|.
                     otherReadMode .|. otherWriteMode)
               total = if ex == IsExecutable then rw .|.
                       ((ownerExecuteMode .|. groupExecuteMode .|.
                         otherExecuteMode)
                        .&. complement umask)
                       else rw
           setFileMode (fn2fp f) total
#endif
    mWriteFilePS = writeAtomicFilePS . fn2fp
    mCreateDirectory = createDirectory . fn2fp
    mCreateFile f = do exf <- mDoesFileExist f
                       if exf then fail $ "File '"++fn2fp f++"' already exists!"
                              else do exd <- mDoesDirectoryExist f
                                      if exd then fail $ "File '"++fn2fp f++"' already exists!"
                                             else mWriteFilePS f B.empty
    mRemoveFile f = do let fp = fn2fp f
                       x <- B.readFile fp
                       if not $ B.null x
                          then fail $ "Cannot remove non-empty file "++fp
                          else removeFile fp
    mRemoveDirectory = removeDirectory . fn2fp
    mRename a b = renameDirectory x y `catch` \_ -> renameFile x y
      where x = fn2fp a
            y = fn2fp b

newtype TolerantIO a = TIO { runTolerantly :: IO a }

warning :: IO () -> TolerantIO ()
warning io = TIO $ io `catch` \e -> putStrLn $ "Warning: " ++ pretty e

runIO :: TolerantIO a -> IO a
runIO (TIO io) = io

runTM :: IO a -> TolerantIO a
runTM io = TIO io

instance Functor TolerantIO where
    fmap f m = m >>= return . f

instance Monad TolerantIO where
    f >>= g = runTM $ runIO f >>= runIO . g
    f >> g = runTM $ runIO f >> runIO g
    fail s = runTM $ fail s
    return x = runTM $ return x

instance WriteableDirectory TolerantIO where
     mDoesDirectoryExist d = runTM $ mDoesDirectoryExist d
     mDoesFileExist f = runTM $ mDoesFileExist f
     mReadFilePS f = runTM $ mReadFilePS f
     mSetFileExecutable f e = warning $ mSetFileExecutable f e
     mWriteFilePS f s = warning $ mWriteFilePS f s
     mCreateFile f = warning $ mWriteFilePS f B.empty
     mCreateDirectory d = warning $ mCreateDirectory d
     mRemoveFile f = warning $ mRemoveFile f
     mRemoveDirectory d = warning $ catchJust ioErrors
                                 (mRemoveDirectory d)
                                 (\e ->
                                   if "(Directory not empty)"
                                          `isSuffixOf` show e
                                   then ioError $ userError $
                                            "Not deleting " ++ fn2fp d ++
                                            " because it is not empty."
                                   else ioError $ userError $
                                            "Not deleting " ++ fn2fp d ++
                                            " because:\n" ++ pretty e)
     mRename a b = warning $ catchJust ioErrors
                          (mRename a b)
                          (\e -> case () of
                                 _ | isPermissionError e -> ioError $ userError $
                                       couldNotRename ++ "."
                                   | isDoesNotExistError e -> ioError $ userError $
                                       couldNotRename ++ " because " ++ x ++ " does not exist."
                                   | otherwise -> ioError e
                          )
       where
        x = fn2fp a
        y = fn2fp b
        couldNotRename = "Could not rename " ++ x ++ " to " ++ y
