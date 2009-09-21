module Main where

import Control.Monad ( when )
import System.Environment ( getArgs )
import System.IO ( IOMode( ReadMode ), hGetContents, hPutStrLn, hClose )
-- import System.IO.Pipe ( openPipe )
import System.Exit ( ExitCode(..) )
import System.Process.Redirects ( createProcess, waitForProcess, proc,
                                  CreateProcess(..), StdStream(..) )

email :: String
email = "roundyd@physics.oregonstate.edu"

lsfiles :: IO [String]
lsfiles =
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-ls-files" []) { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ lines out
         ExitFailure x -> fail "git-ls-files failed"

headhash :: IO String
headhash =
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-show-ref" ["-h"]) { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ cleanhash out
         ExitFailure x -> fail "git-show-ref failed"

updateindex :: [String] -> IO ()
updateindex fs =
    do (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git-update-index" fs)
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure x -> fail "git-update-index failed"

writetree :: IO String
writetree = 
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-write-tree" []) { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ cleanhash out
         ExitFailure x -> fail "git-write-tree failed"

updateref :: String -> String -> IO ()
updateref r h =
    do (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git-update-ref" [r,h])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure x -> fail "git-update-ref failed"

cleanhash :: String -> String
cleanhash = take 40

commit :: String -> IO ()
commit message =
    do fs <- lsfiles
       updateindex fs
       t <- writetree
       putStrLn ("write-tree gives "++show t)
       par <- headhash
       (Just i, Just o, Nothing, pid) <-
           createProcess (proc "git-commit-tree" [t,"-p",par]) {
                  env = Just [("GIT_AUTHOR_NAME","David Roundy"),
                              ("GIT_AUTHOR_EMAIL", email),
                              ("GIT_AUTHOR_DATE","Today"),
                              ("GIT_COMMITTER_NAME","David Roundy"),
                              ("GIT_COMMITTER_EMAIL", email),
                              ("GIT_COMMITTER_DATE","Today")],
                  std_in = CreatePipe,
                  std_out = CreatePipe
                }
       hPutStrLn i message
       out <- hGetContents o
       hClose i
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git-commit-tree failed"
       putStrLn ("commit-tree gives "++ cleanhash out)
       updateref "refs/heads/master" $ cleanhash out

add :: [String] -> IO ()
add fs =
    do (Just stdin, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ("add":fs)) {
                  env = Just [("GIT_AUTHOR_NAME","David Roundy"),
                              ("GIT_AUTHOR_EMAIL", email),
                              ("GIT_AUTHOR_DATE","Today"),
                              ("GIT_COMMITTER_NAME","David Roundy"),
                              ("GIT_COMMITTER_EMAIL", email),
                              ("GIT_COMMITTER_DATE","Today")],
                  std_in = CreatePipe,
                  std_out = CreatePipe
                }
       out <- hGetContents stdout
       hClose stdin
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> do putStrLn out
                           putStrLn "add worked"
         ExitFailure x -> putStrLn out

main = do x <- getArgs
          --h <- openPipe "echo hello world" ReadMode
          --y <- hGetContents h
          putStrLn $ unlines x
          --putStrLn y
          when ("commit" `elem` x) $ commit $ unwords $
               filter (`notElem` ["commit","-m"]) x
          when ("add" `elem` x) $ add $ filter (/= "add") x
