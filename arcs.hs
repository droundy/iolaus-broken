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

commit :: IO ()
commit = do (Just stdin, Just stdout, Nothing, pid) <-
                createProcess (proc "git" ["commit","-a",
                                           "-m","Initial version of arcs.hs"]) {
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
            hPutStrLn stdin "Initial version of arcs.hs"
            hClose stdin
            ec <- length out `seq` waitForProcess pid
            case ec of
              ExitSuccess -> putStrLn "oh goodie"
              ExitFailure x -> putStrLn out

add :: [String] -> IO ()
add fs = do (Just stdin, Just stdout, Nothing, pid) <-
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
          when ("commit" `elem` x) commit
          when ("add" `elem` x) $ add $ filter (/= "add") x
