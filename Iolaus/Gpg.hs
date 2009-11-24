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
import Iolaus.Show ( pretty )
import Git.Plumbing ( committer )

-- TODO: modify verifyGPG to check that things are signed by someone
-- in particular!

verifyGPG :: [Flag] -> String -> String -> IO ()
verifyGPG opts user stxt =
    do x <- world_readable_temp "temp-signature"
       keyring <- ("./"++) `fmap` world_readable_temp "temp-keyring"
       let raw = unlines $ takeWhile (/= "-----BEGIN PGP SIGNATURE-----") $
                 lines stxt
           sig = unlines $ dropWhile (/= "-----BEGIN PGP SIGNATURE-----") $
                 lines stxt
       writeFile x sig
       let fls = (concat [["--no-default-keyring","--keyring",toFilePath kr]
                             | Verify kr <- opts])++
                 ["--batch","--yes","--export","--output",keyring,user]
       debugMessage $ unwords $ "gpg":fls
       (Nothing, Nothing, Nothing, pid1) <- createProcess (proc "gpg" fls)
       ec1 <- waitForProcess pid1
       case ec1 of
         ExitSuccess -> return ()
         ExitFailure _ -> fail ("no public key available for "++user)
       let fls2 = ["--no-default-keyring","--keyring",keyring,"--verify",x,"-"]
       debugMessage $ unwords $ "gpg":fls2
       (Just stdin, Nothing, Nothing, pid) <-
           createProcess (proc "gpg" fls2) { std_in = CreatePipe }
       hPutStr stdin raw
       hClose stdin `catch` \e -> fail ("gpg failed: "++pretty e)
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "gpg failed"

signGPG :: String -> IO String
signGPG txt =
    do args <- (\c -> ["-bsau",c]) `fmap` committer
       debugMessage $ unwords $ "gpg":args
       (Just stdin, Just stdout, Nothing, pid) <-
           createProcess (proc "gpg" args)
                             { std_in = CreatePipe, std_out = CreatePipe }
       hPutStr stdin txt
       hClose stdin `catch` \e -> fail ("gpg failed: "++pretty e)
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return (txt++out)
         ExitFailure _ -> fail "gpg failed"
