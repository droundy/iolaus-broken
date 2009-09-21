module Main where

import Control.Monad ( when )
import System.Environment ( getArgs )
import System.Exit
import System.Process.Redirects ( system )
import System.Directory

import Git.Plumbing ( lsfiles, writetree, updateindex, updateref,
                      diffAllFiles, diffFiles,
                      checkoutCopy, commitTree, headhash )

email :: String
email = "roundyd@physics.oregonstate.edu"

testIndex :: IO ()
testIndex = do checkoutCopy "/tmp/testing/"
               here <- getCurrentDirectory
               setCurrentDirectory "/tmp/testing"
               ec <- system "./.git-hooks/test"
               case ec of
                 ExitFailure _ -> fail "test failed"
                 ExitSuccess -> return ()
               setCurrentDirectory here
               system "rm -rf /tmp/testing"
               return ()

commit :: String -> IO ()
commit message =
    do fs <- lsfiles
       updateindex fs
       testIndex
       t <- writetree
       putStrLn ("write-tree gives "++show t)
       par <- headhash
       com <- commitTree t [par] message
       putStrLn ("commit-tree gives "++ com)
       updateref "refs/heads/master" com

whatsnew :: [FilePath] -> IO ()
whatsnew [] = diffAllFiles >>= putStr
whatsnew fs = diffFiles fs >>= putStr

main :: IO ()
main = do x <- getArgs
          when ("wh" `elem` x) $ whatsnew $ filter (/= "wh") x
          when ("commit" `elem` x) $ commit $ unwords $
               filter (`notElem` ["commit","-m"]) x
          when ("add" `elem` x) $ updateindex $ filter (/= "add") x
