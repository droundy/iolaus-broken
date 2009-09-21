module Git.LocateRepo ( amInRepository, amNotInRepository ) where

import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          doesDirectoryExist )

import Arcs.Flags ( ArcsFlag(..) )
import Arcs.RepoPath ( toFilePath, createDirectoryIfMissing )

amInRepository :: [ArcsFlag] -> IO (Either String ())
amInRepository (WorkDir d:_) =
    do setCurrentDirectory d `catch` (const $ fail $ "can't set directory to "++d)
       air <- currentDirIsRepository
       if air
          then return (Right ())
          else return (Left "You need to be in a repository directory to run this command.")
amInRepository (_:fs) = amInRepository fs
amInRepository [] =
    seekRepo (Left "You need to be in a repository directory to run this command.")

-- | hunt upwards for the darcs repository
-- This keeps changing up one parent directory, testing at each
-- step if the current directory is a repository or not.  $
-- WARNING this changes the current directory for good if matchFn succeeds
seekRepo :: Either String ()
            -- ^ what to return if we don't find a repository
         -> IO (Either String ())
seekRepo onFail = getCurrentDirectory >>= helper where
   helper startpwd = do
    air <- currentDirIsRepository
    if air
       then return (Right ())
       else do cd <- toFilePath `fmap` getCurrentDirectory
               setCurrentDirectory ".."
               cd' <- toFilePath `fmap` getCurrentDirectory
               if cd' /= cd
                  then helper startpwd
                  else do setCurrentDirectory startpwd
                          return onFail

amNotInRepository :: [ArcsFlag] -> IO (Either String ())
amNotInRepository (WorkDir d:_) = do createDirectoryIfMissing False d
                                     -- note that the above could always fail
                                     setCurrentDirectory d
                                     amNotInRepository []
amNotInRepository (_:f) = amNotInRepository f
amNotInRepository [] =
    do air <- currentDirIsRepository
       if air then return (Left $ "You may not run this command in a repository.")
              else return $ Right ()

currentDirIsRepository :: IO Bool
currentDirIsRepository = doesDirectoryExist ".git"
