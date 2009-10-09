{-# LANGUAGE CPP #-}
#include "gadts.h"

module Git.Helpers ( test, slurpTree, writeSlurpTree, touchedFiles,
                     diffCommit, mergeCommits, Strategy(..) ) where

import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          doesFileExist )
import System.Process.Redirects ( system )
import System.Exit ( ExitCode(..) )
import System.IO.Unsafe ( unsafeInterleaveIO )

import Data.ByteString as B ( hPutStr )

import Git.Dag ( mergeBases, makeDag, Dag(..), greatGrandFather )
import Git.Plumbing ( Hash, Tree, Commit, TreeEntry(..),
                      catCommit, CommitEntry(..),
                      mkTree, hashObject, lsothers,
                      diffFiles, DiffOption( NameOnly ),
                      readTree, checkoutIndex,
                      -- mergeBase,
                      mergeIndex, readTreeMerge,
                      catTree, catBlob, catCommitTree )

import Iolaus.Progress ( debugMessage )
import Iolaus.Flags ( IolausFlag(Test) )
import Iolaus.FileName ( FileName, fp2fn )
import Iolaus.IO ( ExecutableBit(..) )
import Iolaus.SlurpDirectoryInternal
    ( Slurpy(..), SlurpyContents(..), empty_slurpy,
      slurpies_to_map, map_to_slurpies )
import Iolaus.Lock ( removeFileMayNotExist )
import Iolaus.Diff ( diff )
import Iolaus.Patch ( Prim, apply_to_slurpy, mergeN )
import Iolaus.Ordered ( FL(..), (+>+) )
import Iolaus.Sealed ( Sealed(..), FlippedSeal(..), mapSealM )

#include "impossible.h"

touchedFiles :: IO [FilePath]
touchedFiles =
    do x <- lsothers
       y <- diffFiles [NameOnly] []
       return (x++lines y)

test :: [IolausFlag] -> Hash Tree C(x) -> IO ()
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

slurpTree :: Hash Tree C(x) -> IO (Slurpy C(x))
slurpTree = slurpTreeHelper (fp2fn ".")

slurpTreeHelper :: FileName -> Hash Tree C(x) -> IO (Slurpy C(x))
slurpTreeHelper rootdir t =
    do xs <- catTree t
       unsafeInterleaveIO $
             (Slurpy rootdir . SlurpDir (Just t). slurpies_to_map)
             `fmap` mapM sl xs
    where sl (n, Subtree t') = unsafeInterleaveIO $ slurpTreeHelper n t'
          sl (n, File h) =
              do x <- unsafeInterleaveIO $ catBlob h
                 return $ Slurpy n $ SlurpFile NotExecutable (Just h) x
          sl (n, Executable h) =
              do x <- unsafeInterleaveIO $ catBlob h
                 return $ Slurpy n $ SlurpFile IsExecutable (Just h) x
          sl (n, Symlink h) =
              do x <- unsafeInterleaveIO $ catBlob h
                 return $ Slurpy n $ SlurpSymlink x

writeSlurpTree :: Slurpy C(x) -> IO (Hash Tree C(x))
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

data Strategy = FirstParent | Builtin | MergeN

mergeCommits :: Strategy -> [Sealed (Hash Commit)] -> IO (Sealed (Hash Tree))
mergeCommits _ [] = Sealed `fmap` writeSlurpTree empty_slurpy
mergeCommits _ [Sealed h] = Sealed `fmap` catCommitTree h
mergeCommits FirstParent (Sealed h:_) = Sealed `fmap` catCommitTree h
mergeCommits Builtin [p1,p2] =
    do -- ancestor <- mergeBase p1 p2
       let ancestor:_ = mergeBases [p1,p2]
       [Sealed ta,Sealed t1,Sealed t2] <-
           mapM (\(Sealed x) -> Sealed `fmap` catCommitTree x)
                     [ancestor,p1,p2]
       readTreeMerge ta t1 t2 "merging"
       mergeIndex "merging"
mergeCommits Builtin _ = fail "Builtin can't do octopi"
mergeCommits MergeN xs =
    case mergeBases xs of
      [] -> fail "FIXME: need to implement merge with no common ancestor."
      Sealed ancestor:_ ->
          do pps <- mapM (mapSealM (bigDiff ancestor)) xs
             Sealed ps <- return $ mergeN pps
             oldest <- catCommitTree ancestor >>= slurpTree
             Sealed `fmap` writeSlurpTree (fromJust $ apply_to_slurpy ps oldest)

bigDiff :: Hash Commit C(x) -> Hash Commit C(y) -> IO (FL Prim C(x y))
bigDiff a0 me0 = diffDag $ makeDag a0 me0

diffDag :: Dag C(x y) -> IO (FL Prim C(x y))
diffDag (Ancestor _) = return NilFL
diffDag (Node x [Sealed (Ancestor y)]) =
    do old <- catCommitTree y >>= slurpTree
       new <- catCommitTree x >>= slurpTree
       return $ diff [] old new
diffDag (Node x [Sealed y@(Node yy _)]) =
    do old <- catCommitTree yy >>= slurpTree
       new <- catCommitTree x >>= slurpTree
       oldps <- diffDag y
       return $ oldps +>+ diff [] old new
diffDag (Node x ys) =
    do oldest <- catCommitTree (greatGrandFather (Node x ys)) >>= slurpTree
       new <- catCommitTree x >>= slurpTree
       tracks <- mapM (mapSealM diffDag) ys
       Sealed oldps <- return $ mergeN tracks
       let Just old = apply_to_slurpy oldps oldest
       return $ oldps +>+ diff [] old new

diffCommit :: Strategy -> Hash Commit C(x) -> IO (FlippedSeal (FL Prim) C(x))
diffCommit strat c0 =
    do c <- catCommit c0
       new <- slurpTree $ myTree c
       Sealed oldh <- mergeCommits strat (myParents c)
       old <- slurpTree oldh
       return $ FlippedSeal $ diff [] old new
