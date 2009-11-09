{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Iolaus.Compat (stdout_is_a_pipe, mk_stdout_temp, canonFilename,
                    sloppy_atomic_create) where

import Prelude hiding ( catch )

import Iolaus.Utils ( withCurrentDirectory )
#ifdef WIN32
import Iolaus.Utils ( showHexLen )
import Data.Bits ( (.&.) )
import System.Random ( randomIO )
#else
import Foreign.C.String ( peekCString )
#endif

import Foreign.C.Types ( CInt )
import Foreign.C.String ( CString, withCString )
import Foreign.C.Error ( throwErrno )
import System.Directory ( getCurrentDirectory )
import System.IO ( hFlush, stdout, stderr, hSetBuffering,
                   BufferMode(NoBuffering) )
import System.Posix.Files ( stdFileMode )
import System.Posix.IO ( openFd, closeFd, stdOutput, stdError, 
                         dupTo, defaultFileFlags, exclusive,
                         OpenMode(WriteOnly) )
import System.Posix.Types ( Fd(..) )

import Iolaus.SignalHandler ( stdout_is_a_pipe )

canonFilename :: FilePath -> IO FilePath
canonFilename f@(_:':':_) = return f -- absolute windows paths
canonFilename f@('/':_) = return f
canonFilename ('.':'/':f) = do cd <- getCurrentDirectory
                               return $ cd ++ "/" ++ f
canonFilename f = case reverse $ dropWhile (/='/') $ reverse f of
                  "" -> fmap (++('/':f)) getCurrentDirectory
                  rd -> withCurrentDirectory rd $
                          do fd <- getCurrentDirectory
                             return $ fd ++ "/" ++ simplefilename
    where
    simplefilename = reverse $ takeWhile (/='/') $ reverse f

#ifdef WIN32
mkstemp_core :: FilePath -> IO (Fd, String)
mkstemp_core fp
 = do r <- randomIO
      let fp' = fp ++ (showHexLen 6 (r .&. 0xFFFFFF :: Int))
      fd <- openFd fp' WriteOnly (Just stdFileMode) flags
      return (fd, fp')
  where flags = defaultFileFlags { exclusive = True }
#else
mkstemp_core :: String -> IO (Fd, String)
mkstemp_core str = withCString (str++"XXXXXX") $
    \cstr -> do fd <- c_mkstemp cstr
                if fd < 0
                  then throwErrno $ "Failed to create temporary file "++str
                  else do str' <- peekCString cstr
                          fname <- canonFilename str'
                          return (Fd fd, fname)

foreign import ccall unsafe "static stdlib.h mkstemp"
    c_mkstemp :: CString -> IO CInt
#endif

mk_stdout_temp :: String -> IO String
mk_stdout_temp str = do (fd, fn) <- mkstemp_core str
                        hFlush stdout
                        hFlush stderr
                        dupTo fd stdOutput
                        dupTo fd stdError
                        hFlush stdout
                        hFlush stderr
                        hSetBuffering stdout NoBuffering
                        hSetBuffering stderr NoBuffering
                        return fn

sloppy_atomic_create :: FilePath -> IO ()
sloppy_atomic_create fp
    = do fd <- openFd fp WriteOnly (Just stdFileMode) flags
         closeFd fd
  where flags = defaultFileFlags { exclusive = True }
