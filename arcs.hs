module Main where

import Control.Monad ( when )
import System.Environment ( getArgs )

import Git.Plumbing ( lsfiles, writetree, updateindex, updateref,
                      commitTree, headhash )

email :: String
email = "roundyd@physics.oregonstate.edu"

commit :: String -> IO ()
commit message =
    do fs <- lsfiles
       updateindex fs
       t <- writetree
       putStrLn ("write-tree gives "++show t)
       par <- headhash
       com <- commitTree t [par] message
       putStrLn ("commit-tree gives "++ com)
       updateref "refs/heads/master" com

main :: IO ()
main = do x <- getArgs
          --h <- openPipe "echo hello world" ReadMode
          --y <- hGetContents h
          putStrLn $ unlines x
          --putStrLn y
          when ("commit" `elem` x) $ commit $ unwords $
               filter (`notElem` ["commit","-m"]) x
          when ("add" `elem` x) $ updateindex $ filter (/= "add") x
