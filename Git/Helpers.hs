{-# LANGUAGE CPP #-}
#include "gadts.h"

module Git.Helpers ( test, testPredicate, revListHeads,
                     slurpTree, writeSlurpTree, touchedFiles,
                     simplifyParents,
                     diffCommit, mergeCommits, Strategy(..) ) where

import Prelude hiding ( catch )
import Control.Exception ( catch )
import System.Directory ( getCurrentDirectory, setCurrentDirectory,
                          doesFileExist )
import System.Process.Redirects ( system )
import System.Exit ( ExitCode(..) )
import System.IO ( hPutStrLn )
import System.IO.Unsafe ( unsafeInterleaveIO )
import Data.List ( sort )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import Data.Map as M ( Map, insert, empty, lookup, keys )
import Data.ByteString as B ( hPutStr )

import Git.Dag ( mergeBases, makeDag, Dag(..), greatGrandFather, parents,
                 cauterizeHeads, dag2commit )
import Git.Plumbing ( Hash, Tree, Commit, TreeEntry(..),
                      catCommit, CommitEntry(..),
                      commitTree, updateref, parseRev,
                      mkTree, hashObject, lsothers,
                      diffFiles, DiffOption( NameOnly ),
                      readTree, checkoutIndex,
                      heads, revList, RevListOption(Skip),
                      -- mergeBase,
                      mergeIndex, readTreeMerge,
                      catTree, catBlob, catCommitTree )

import Iolaus.Progress ( debugMessage )
import Iolaus.Flags ( Flag( Test, TestParents, NativeMerge, FirstParentMerge,
                            IolausSloppyMerge, CauterizeAllHeads ) )
import Iolaus.FileName ( FileName, fp2fn )
import Iolaus.IO ( ExecutableBit(..) )
import Iolaus.SlurpDirectoryInternal
    ( Slurpy(..), SlurpyContents(..), empty_slurpy,
      slurpies_to_map, map_to_slurpies )
import Iolaus.Lock ( removeFileMayNotExist )
import Iolaus.Diff ( diff )
import Iolaus.Patch ( Prim, commute, apply_to_slurpy, mergeN )
import Iolaus.Ordered ( FL(..), (:>)(..), (+>+), unsafeCoerceP )
import Iolaus.Sealed ( Sealed(..), FlippedSeal(..), mapSeal, mapSealM, unseal )

#include "impossible.h"

touchedFiles :: IO [FilePath]
touchedFiles =
    do x <- lsothers
       y <- diffFiles [NameOnly] []
       return (x++lines y)

test :: [Flag] -> Hash Tree C(x) -> IO ()
test opts t =
    do x <- testPredicate opts t
       if x then return ()
            else fail "test failed"

testPredicate :: [Flag] -> Hash Tree C(x) -> IO Bool
testPredicate opts t | Test `elem` opts || TestParents `elem` opts =
 do havet <- doesFileExist ".git-hooks/test"
    if not havet
     then return True
     else do
       system "rm -rf /tmp/testing"
       removeFileMayNotExist ".git/index.tmp"
       readTree t "index.tmp"
       checkoutIndex "index.tmp" "/tmp/testing/"
       removeFileMayNotExist ".git/index.tmp"
       here <- getCurrentDirectory
       setCurrentDirectory "/tmp/testing"
       ec <- system "./.git-hooks/test"
       setCurrentDirectory here
       case ec of
         ExitFailure _ -> return False
         ExitSuccess -> do system "rm -rf /tmp/testing"
                           return True
testPredicate _ _ = return True

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

data Strategy = FirstParent | Builtin | MergeN | MergeNSloppy
                deriving ( Show )

flag2strategy :: [Flag] -> Strategy
flag2strategy opts = if NativeMerge `elem` opts
                     then Builtin
                     else if FirstParentMerge `elem` opts
                          then FirstParent
                          else if IolausSloppyMerge `elem` opts
                               then MergeNSloppy
                               else MergeN

simplifyParents :: [Flag] -> [Sealed (Hash Commit)] -> Hash Tree C(x)
                -> IO ([Sealed (Hash Commit)], Sealed (Hash Tree))
simplifyParents opts pars0 rec0
    | CauterizeAllHeads `elem` opts = return (pars0, Sealed rec0)
simplifyParents opts pars0 rec0 = sp [] (cauterizeHeads pars0) rec0
    where
      sp :: [Sealed (Hash Commit)] -> [Sealed (Hash Commit)] -> Hash Tree C(x)
         -> IO ([Sealed (Hash Commit)], Sealed (Hash Tree))
      sp ps [] t = return (cauterizeHeads ps,Sealed t)
      sp kn (p:ps) t =
          do let nop = cauterizeHeads (kn++ps++unseal parents p)
             Sealed ptree <- mergeCommits opts (kn++p:ps)
                             >>= mapSealM slurpTree
             Sealed noptree <- mergeCommits opts nop >>= mapSealM slurpTree
             mys <- slurpTree t
             case commute (diff opts noptree ptree :> diff opts ptree mys) of
               Nothing -> sp (p:kn) ps t
               Just (myp :> _) ->
                   do t' <- apply_to_slurpy myp noptree >>= writeSlurpTree
                      ct <- commitTree t (p:kn++ps) "iolaus:testing"
                      joined0 <- mergeCommitsX MergeN (Sealed ct:p:nop)
                      ct' <- commitTree t' nop "iolaus:testing"
                      joined <- mergeCommitsX MergeN (Sealed ct':p:nop)
                      ok <-
                          if joined == joined0
                          then
                            if TestParents `elem` opts
                            then do Sealed x <- mapSealM catCommit p
                                    putStrLn $ "\n\nRunning test without:\n"++
                                             myMessage x
                                    testPredicate opts t'
                            else return True
                          else return False
                      if ok then sp kn (filter (`notElem` kn) nop) t'
                            else sp (p:kn) ps t

mergeCommits :: [Flag] -> [Sealed (Hash Commit)]
             -> IO (Sealed (Hash Tree))
mergeCommits opts = mergeCommits0 (flag2strategy opts)

mergeCommits0 :: Strategy -> [Sealed (Hash Commit)]
             -> IO (Sealed (Hash Tree))
mergeCommits0 s hs0 =
    do let hs = cauterizeHeads hs0
       mt <- readCached s hs0
       case mt of
         Just t -> return t
         Nothing -> do Sealed t <- mergeCommitsX s hs
                       cacheTree s hs t
                       return (Sealed t)

mergeCommitsX :: Strategy -> [Sealed (Hash Commit)] -> IO (Sealed (Hash Tree))
mergeCommitsX _ [] = Sealed `fmap` writeSlurpTree empty_slurpy
mergeCommitsX _ [Sealed h] = Sealed `fmap` catCommitTree h
mergeCommitsX FirstParent (Sealed h:_) = Sealed `fmap` catCommitTree h
mergeCommitsX Builtin [p1,p2] =
    do -- ancestor <- mergeBase p1 p2
       let ancestor:_ = mergeBases [p1,p2]
       [Sealed ta,Sealed t1,Sealed t2] <-
           mapM (\(Sealed x) -> Sealed `fmap` catCommitTree x)
                     [ancestor,p1,p2]
       readTreeMerge ta t1 t2 "merging"
       mergeIndex "merging"
mergeCommitsX Builtin _ = fail "Builtin can't do octopi"
mergeCommitsX s xs = -- this is MergeN*
    case mergeBases xs of
      [] -> do ts <- mapM (mapSealM catCommitTree) xs
               sls <- mapM (mapSealM slurpTree) ts
               Sealed p <- return $ mergeN $
                           map (mapSeal (diff [] empty_slurpy)) sls
               Sealed `fmap` writeSlurpTree
                          (fromJust $ apply_to_slurpy p empty_slurpy)
      Sealed ancestor:_ ->
          do diffs <- newIORef M.empty
             pps <- mapM (mapSealM (bigDiff diffs s ancestor)) xs
             Sealed ps <- return $ mergeN pps
             oldest <- catCommitTree ancestor >>= slurpTree
             Sealed `fmap` writeSlurpTree (fromJust $ apply_to_slurpy ps oldest)

cacheTree :: Strategy -> [Sealed (Hash Commit)] -> Hash Tree C(x) -> IO ()
cacheTree s x t =
    do k <- hashObject (`hPutStrLn` show (s, sort x))
       commitTree t (sort x) (show $ sort x)
                      >>= updateref ("refs/merges/"++show k) 
       return ()

readCached :: Strategy -> [Sealed (Hash Commit)]
           -> IO (Maybe (Sealed (Hash Tree)))
readCached s x =
    do k <- hashObject (`hPutStrLn` show (s, sort x))
       Just `fmap`
                (parseRev ("refs/merges/"++show k) >>= mapSealM catCommitTree)
    `catch` \_ -> return Nothing

bigDiff :: IORef (M.Map (Sealed (Hash Commit)) (Sealed (FL Prim C(x))))
        -> Strategy
        -> Hash Commit C(x) -> Hash Commit C(y) -> IO (FL Prim C(x y))
bigDiff dags strat a0 me0 =
    maybe (error "bad bigDiff") (diffDag dags strat) $ makeDag a0 me0

diffDag :: IORef (M.Map (Sealed (Hash Commit)) (Sealed (FL Prim C(x))))
        -> Strategy -> Dag C(x y) -> IO (FL Prim C(x y))
diffDag _ _ (Ancestor _) = return NilFL
diffDag c s (Node x ys) =
    do m <- readIORef c
       case M.lookup (Sealed x) m of
         Just (Sealed ps) -> return $ unsafeCoerceP ps
         Nothing -> do ps <- diffDagHelper c s (Node x ys)
                       modifyIORef c (M.insert (Sealed x) (Sealed ps))
                       return ps

diffDagHelper :: IORef (M.Map (Sealed (Hash Commit)) (Sealed (FL Prim C(x))))
              -> Strategy -> Dag C(x y) -> IO (FL Prim C(x y))
diffDagHelper _ _ (Ancestor _) = return NilFL
diffDagHelper _ _ (Node x [Sealed (Ancestor y)]) =
    do old <- catCommitTree y >>= slurpTree
       new <- catCommitTree x >>= slurpTree
       return $ diff [] old new
diffDagHelper c s (Node x [Sealed y@(Node yy _)]) =
    do old <- catCommitTree yy >>= slurpTree
       new <- catCommitTree x >>= slurpTree
       oldps <- diffDag c s y
       return $ oldps +>+ diff [] old new
diffDagHelper c MergeN (Node x ys) =
    do oldest <- catCommitTree (greatGrandFather (Node x ys)) >>= slurpTree
       new <- catCommitTree x >>= slurpTree
       tracks <- mapM (mapSealM (diffDag c MergeN)) ys
       Sealed oldps <- return $ mergeN tracks
       let Just old = apply_to_slurpy oldps oldest
       return $ oldps +>+ diff [] old new
diffDagHelper c s (Node x ys@(Sealed y0:_)) = -- this version is buggy!!!
    do Sealed merged <- mergeCommits0 s (map (mapSeal dag2commit) ys)
                        >>= mapSealM slurpTree
       ps0 <- diffDag c s y0
       old <- catCommitTree (dag2commit y0) >>= slurpTree
       new <- catCommitTree x >>= slurpTree
       tracks <- mapM (mapSealM (diffDag c s)) ys
       return $ ps0 +>+ diff [] old merged +>+ diff [] merged new

diffCommit :: [Flag] -> Hash Commit C(x)
           -> IO (FlippedSeal (FL Prim) C(x))
diffCommit opts c0 =
    do c <- catCommit c0
       new <- slurpTree $ myTree c
       Sealed oldh <- mergeCommits opts (myParents c)
       old <- slurpTree oldh
       return $ FlippedSeal $ diff [] old new

revListHeads :: [Flag] -> [RevListOption] -> IO String
revListHeads opts revlistopts =
    do hs <- cauterizeHeads `fmap` heads
       Sealed t <- mergeCommits opts hs
       c <- commitTree t hs "iolaus:temp"
       revList (show c) (Skip 1:revlistopts)
