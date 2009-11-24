{-# LANGUAGE CPP #-}

module Iolaus.Gpg ( signGPG, verifyGPG ) where

#ifdef HAVE_REDIRECTS
import System.Process.Redirects ( createProcess, waitForProcess, proc,
                                  CreateProcess(..), StdStream(..) )
#else
import System.Process ( createProcess, waitForProcess, proc,
                        CreateProcess(..), StdStream(..) )
#endif
import System.IO ( hClose, hPutStr, hGetContents )
import System.Exit ( ExitCode(..) )

import Iolaus.RepoPath ( toFilePath )
import Iolaus.Flags ( Flag(Verify) )
import Iolaus.Global ( debugMessage )
import Iolaus.Lock ( world_readable_temp )
import Git.Plumbing ( committer )

-- TODO: modify verifyGPG to check that things are signed by someone
-- in particular!

verifyGPG :: [Flag] -> String -> IO ()
verifyGPG opts stxt =
    do x <- world_readable_temp "gpg-verify-this-please"
       let raw = unlines $ takeWhile (/= "-----BEGIN PGP SIGNATURE-----") $
                 lines stxt
           sig = unlines $ dropWhile (/= "-----BEGIN PGP SIGNATURE-----") $
                 lines stxt
       debugMessage $ "raw is "++raw
       debugMessage $ "sig is "++sig
       writeFile x sig
       let fls = concat [["--no-default-keyring","--keyring",toFilePath kr]
                             | Verify kr <- opts]
       debugMessage $ unwords $ "calling":fls++["--verify",x,"-"]
       (Just stdin, Nothing, Nothing, pid) <-
           createProcess (proc "gpg" (fls++["--verify",x,"-"]))
                             { std_in = CreatePipe }
       hPutStr stdin raw
       hClose stdin
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "gpg failed"

signGPG :: String -> IO String
signGPG txt =
    do debugMessage "calling gpg"
       myid <- committer
       (Just stdin, Just stdout, Nothing, pid) <-
           createProcess (proc "gpg" ["-bsau",myid])
                             { std_in = CreatePipe, std_out = CreatePipe }
       hPutStr stdin txt
       hClose stdin
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return (txt++out)
         ExitFailure _ -> fail "gpg failed"
