{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}

module Arcs.Workaround ( renameFile, setExecutable, getCurrentDirectory,
                    installHandler, raiseSignal, Handler(..), Signal,
                    sigINT, sigHUP, sigABRT, sigALRM, sigTERM, sigPIPE ) where

#ifdef WIN32
import qualified System.Directory ( renameFile, getCurrentDirectory, removeFile )
import qualified Control.Exception ( block )
import qualified System.IO.Error ( isDoesNotExistError, ioError, catch )
#else
import System.Posix.Signals(installHandler, raiseSignal, Handler(..), Signal,
                            sigINT, sigHUP, sigABRT, sigALRM, sigTERM, sigPIPE)
import System.Directory ( renameFile, getCurrentDirectory )
import System.Posix.Files (fileMode,getFileStatus, setFileMode,
                           setFileCreationMask,
                           ownerReadMode, ownerWriteMode, ownerExecuteMode,
                           groupReadMode, groupWriteMode, groupExecuteMode,
                           otherReadMode, otherWriteMode, otherExecuteMode)
import Data.Bits ( (.&.), (.|.), complement )
#endif

#ifdef WIN32
-- Dummy implementation of POSIX signals
data Handler = Default | Ignore | Catch (IO ())
type Signal = Int

installHandler :: Signal -> Handler -> Maybe () -> IO ()
installHandler _ _ _ = return ()

raiseSignal :: Signal -> IO ()
raiseSignal _ = return ()

sigINT, {- sigKILL, -} sigHUP, {- sigQUIT, -} sigABRT, sigALRM, sigTERM, sigPIPE :: Signal
sigINT = 0
-- not used: sigKILL = 0
sigHUP = 0
-- not used: sigQUIT = 0
sigABRT = 0
sigTERM = 0
sigPIPE = 0
sigALRM = 0

{-
System.Directory.renameFile incorrectly fails when the new file already
exists.  This code works around that bug at the cost of losing atomic
writes.
-}

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

setExecutable :: FilePath -> Bool -> IO ()
setExecutable _ _ = return ()

{-
System.Directory.getCurrentDirectory returns a path with backslashes in it
under windows, and some of the code gets confused by that, so we override
getCurrentDirectory and translates '\\' to '/'
-}

getCurrentDirectory :: IO FilePath
getCurrentDirectory = do d <- System.Directory.getCurrentDirectory
                         return $ map rb d
    where rb '\\' = '/'
          rb c = c

#else

setExecutable :: FilePath -> Bool -> IO ()
setExecutable f ex =
  do st <- getFileStatus f
     umask <- setFileCreationMask 0
     setFileCreationMask umask
     let rw = fileMode st .&.
              (ownerReadMode .|. ownerWriteMode .|.
               groupReadMode .|. groupWriteMode .|.
               otherReadMode .|. otherWriteMode)
         total = if ex then rw .|.
                           ((ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode)
                            .&. complement umask)
                       else rw
     setFileMode f total

#endif
