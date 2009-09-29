module Git.Helpers ( test, slurpTree, writeSlurpTree, touchedFiles ) where

import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          doesFileExist )
import System.Process.Redirects ( system )
import System.Exit ( ExitCode(..) )
import System.IO.Unsafe ( unsafeInterleaveIO )

import Data.ByteString as B ( hPutStr )

import Git.Plumbing ( Hash, Tree, TreeEntry(..),
                      mkTree, hashObject, lsothers,
                      diffFiles, DiffOption( NameOnly ),
                      readTree, checkoutIndex,
                      catTree, catBlob )

import Grit.Progress ( debugMessage )
import Grit.Flags ( GritFlag(Test) )
import Grit.FileName ( FileName, fp2fn )
import Grit.IO ( ExecutableBit(..) )
import Grit.SlurpDirectoryInternal ( Slurpy(..), SlurpyContents(..),
                                     slurpies_to_map, map_to_slurpies )
import Grit.Lock ( removeFileMayNotExist )

touchedFiles :: IO [FilePath]
touchedFiles =
    do x <- lsothers
       y <- diffFiles [NameOnly] []
       return (x++lines y)

test :: [GritFlag] -> Hash Tree -> IO ()
test opts t | Test `elem` opts =
 do havet <- doesFileExist ".git-hooks/test"
    if not havet
     then return ()
     else do
       system "rm -rf /tmp/testing"
       removeFileMayNotExist ".git/index.tmp"
       readTree t "index.tmp"
       checkoutIndex "index.tmp" "/tmp/testing/"
       removeFileMayNotExist ".git/index.tmp"
       here <- getCurrentDirectory
       setCurrentDirectory "/tmp/testing"
       ec <- system "./.git-hooks/test"
       case ec of
         ExitFailure _ -> fail "test failed"
         ExitSuccess -> return ()
       setCurrentDirectory here
       system "rm -rf /tmp/testing"
       return ()
test _ _ = return ()

slurpTree :: FileName -> Hash Tree -> IO Slurpy
slurpTree rootdir t =
    do xs <- catTree t
       unsafeInterleaveIO $
             (Slurpy rootdir . SlurpDir (Just t). slurpies_to_map)
             `fmap` mapM sl xs
    where sl (n, Subtree t') = unsafeInterleaveIO $ slurpTree n t'
          sl (n, File h) =
              do x <- unsafeInterleaveIO $ catBlob h
                 return $ Slurpy n $ SlurpFile NotExecutable (Just h) x
          sl (n, Executable h) =
              do x <- unsafeInterleaveIO $ catBlob h
                 return $ Slurpy n $ SlurpFile IsExecutable (Just h) x

writeSlurpTree :: Slurpy -> IO (Hash Tree)
writeSlurpTree (Slurpy _ (SlurpDir (Just t) _)) = return t
writeSlurpTree (Slurpy _ (SlurpDir Nothing ccc)) =
    do debugMessage "starting writeSlurpTree"
       tes <- mapM writeSubsl $ map_to_slurpies ccc
       mkTree tes
    where writeSubsl (Slurpy fn (SlurpFile IsExecutable (Just h) _)) =
              return (fn, Executable h)
          writeSubsl (Slurpy fn (SlurpFile IsExecutable Nothing c)) =
              do h <- hashObject (`B.hPutStr` c)
                 return (fn, Executable h)
          writeSubsl (Slurpy fn (SlurpFile NotExecutable (Just h) _)) =
              return (fn, File h)
          writeSubsl (Slurpy fn (SlurpFile NotExecutable Nothing c)) =
              do h <- hashObject (`B.hPutStr` c)
                 return (fn, File h)
          writeSubsl (Slurpy fn (SlurpDir (Just h) _)) =
              return (fn, Subtree h)
          writeSubsl d@(Slurpy fn (SlurpDir Nothing _)) =
              do h <- writeSlurpTree d
                 return (fn, Subtree h)
writeSlurpTree x = writeSlurpTree (Slurpy (fp2fn ".")
                                    (SlurpDir Nothing $ slurpies_to_map [x]))
