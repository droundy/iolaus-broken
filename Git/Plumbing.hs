module Git.Plumbing ( lsfiles, updateindex, writetree, updateref,
                      headhash, commitTree ) where

import System.IO ( hGetContents, hPutStr, hClose )
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

commitTree :: String -> [String] -> String -> IO String
commitTree t pars m =
    do let pflags = concatMap (\p -> ["-p",p]) pars
       (Just i, Just o, Nothing, pid) <-
           createProcess (proc "git-commit-tree" (t:pflags)) {
               env = Just [("GIT_AUTHOR_NAME","David Roundy"),
                           ("GIT_AUTHOR_EMAIL", "droundy@abridgegame.org"),
                           ("GIT_COMMITTER_NAME","David Roundy"),
                           ("GIT_COMMITTER_EMAIL", "droundy@abridgegame.org")],
               std_out = CreatePipe,
               std_in = CreatePipe }
       out <- hGetContents o
       hPutStr i m
       hClose i
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ cleanhash out
         ExitFailure _ -> fail "git-commit-tree failed"


cleanhash :: String -> String
cleanhash = take 40
