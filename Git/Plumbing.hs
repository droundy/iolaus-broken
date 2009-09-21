module Git.Plumbing ( lsfiles, updateindex, writetree, updateref,
                      headhash,
                      cleanhash ) where

import System.IO ( hGetContents )
-- import System.IO.Pipe ( openPipe )
import System.Exit ( ExitCode(..) )
import System.Process.Redirects ( createProcess, waitForProcess, proc,
                                  CreateProcess(..), StdStream(..) )

lsfiles :: IO [String]
lsfiles =
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-ls-files" []) { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ lines out
         ExitFailure _ -> fail "git-ls-files failed"

headhash :: IO String
headhash =
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-show-ref" ["-h"]) { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ cleanhash out
         ExitFailure _ -> fail "git-show-ref failed"

updateindex :: [String] -> IO ()
updateindex fs =
    do (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git-update-index" ("--add":"--remove":"--":fs))
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git-update-index failed"

writetree :: IO String
writetree = 
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-write-tree" []) { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ cleanhash out
         ExitFailure _ -> fail "git-write-tree failed"

updateref :: String -> String -> IO ()
updateref r h =
    do (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git-update-ref" [r,h])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git-update-ref failed"

cleanhash :: String -> String
cleanhash = take 40
