module Git.Helpers ( test, slurpTree, writeSlurpTree, touchedFiles,
                     diffCommit, mergeCommits, Strategy(..) ) where

import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          doesFileExist )
import System.Process.Redirects ( system )
import System.Exit ( ExitCode(..) )
import System.IO.Unsafe ( unsafeInterleaveIO )

import Data.ByteString as B ( hPutStr )

import Git.Plumbing ( Hash, Tree, Commit, TreeEntry(..),
                      catCommit, CommitEntry(..),
                      mkTree, hashObject, lsothers,
                      diffFiles, DiffOption( NameOnly ),
                      readTree, checkoutIndex,
                      mergeBase, mergeIndex, readTreeMerge,
                      catTree, catBlob, catCommitTree )

import Iolaus.Progress ( debugMessage )
import Iolaus.Flags ( IolausFlag(Test) )
import Iolaus.FileName ( FileName, fp2fn )
import Iolaus.IO ( ExecutableBit(..) )
import Iolaus.SlurpDirectoryInternal
    ( Slurpy(..), SlurpyContents(..), empty_slurpy,
      slurpies_to_map, map_to_slurpies )
import Iolaus.Lock ( removeFileMayNotExist )
import Iolaus.Diff ( unsafeDiff )
import Iolaus.Patch ( Prim )
import Iolaus.Ordered ( FL )

touchedFiles :: IO [FilePath]
touchedFiles =
    do x <- lsothers
       y <- diffFiles [NameOnly] []
       return (x++lines y)

test :: [IolausFlag] -> Hash Tree -> IO ()
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
          sl (n, Symlink h) =
              do x <- unsafeInterleaveIO $ catBlob h
                 return $ Slurpy n $ SlurpSymlink x

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
          writeSubsl (Slurpy fn (SlurpSymlink x)) =
              do h <- hashObject (`B.hPutStr` x)
                 return (fn, Symlink h)
          writeSubsl (Slurpy fn (SlurpDir (Just h) _)) =
              return (fn, Subtree h)
          writeSubsl d@(Slurpy fn (SlurpDir Nothing _)) =
              do h <- writeSlurpTree d
                 return (fn, Subtree h)
writeSlurpTree x = writeSlurpTree (Slurpy (fp2fn ".")
                                    (SlurpDir Nothing $ slurpies_to_map [x]))

data Strategy = FirstParent | Builtin

mergeCommits :: Strategy -> [Hash Commit] -> IO (Hash Tree)
mergeCommits _ [] = writeSlurpTree empty_slurpy
mergeCommits _ [h] = catCommitTree h
mergeCommits FirstParent (h:_) = catCommitTree h
mergeCommits Builtin [p1,p2] =
    do ancestor <- mergeBase p1 p2
       [ta,t1,t2] <- mapM catCommitTree [ancestor,p1,p2]
       readTreeMerge ta t1 t2 "merging"
       mergeIndex "merging"
mergeCommits Builtin _ = fail "Builtin can't do octopi"

diffCommit :: Strategy -> Hash Commit -> IO (FL Prim)
diffCommit strat c0 =
    do c <- catCommit c0
       new <- slurpTree (fp2fn ".") $ myTree c
       old <- mergeCommits strat (myParents c) >>= slurpTree (fp2fn ".")
       return $ unsafeDiff [] old new
