module Main where

import Control.Monad ( when )
import System.Environment ( getArgs )
import System.IO ( hGetContents, hPutStrLn, hClose )
-- import System.IO.Pipe ( openPipe )
import System.Exit ( ExitCode(..) )
import System.Process.Redirects ( createProcess, waitForProcess, proc,
                                  CreateProcess(..), StdStream(..) )

import Git.Plumbing ( lsfiles, writetree, updateindex, updateref,
                      headhash, cleanhash )

email :: String
email = "roundyd@physics.oregonstate.edu"

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

main :: IO ()
main = do x <- getArgs
          --h <- openPipe "echo hello world" ReadMode
          --y <- hGetContents h
          putStrLn $ unlines x
          --putStrLn y
          when ("commit" `elem` x) $ commit $ unwords $
               filter (`notElem` ["commit","-m"]) x
          when ("add" `elem` x) $ updateindex $ filter (/= "add") x
