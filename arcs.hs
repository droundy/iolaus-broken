module Main where

import Control.Monad ( when )
import System.Environment ( getArgs )

import Git.Plumbing ( lsfiles, writetree, updateindex, updateref,
                      diffAllFiles, diffFiles,
                      commitTree, headhash )
import Git.Helpers ( testIndex )

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
