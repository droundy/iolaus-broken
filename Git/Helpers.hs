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
#ifndef HAVE_REDIRECTS
import System.Cmd ( system )
#else
import System.Process.Redirects ( system )
#endif
import System.Exit ( ExitCode(..) )
import System.IO ( hPutStrLn )
import System.IO.Unsafe ( unsafeInterleaveIO )
import Data.List ( sort )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import Data.Map as M ( Map, insert, empty, lookup )
import Data.ByteString as B ( hPutStr )

import Git.Dag ( mergeBases, makeDag, Dag(..), greatGrandFather, parents,
                 cauterizeHeads, dag2commit )
import Git.Plumbing ( Hash, Tree, Commit, TreeEntry(..),
                      catCommit, CommitEntry(..),
                      commitTree, updateref, parseRev,
                      mkTree, hashObject, lsothers,
                      diffFiles, diffTreeCommit,
                      DiffOption( NameOnly, DiffRecursive ),
                      readTree, checkoutIndex,
                      heads, revList, RevListOption(Skip),
                      -- mergeBase,
                      mergeIndex, readTreeMerge,
                      catTree, catBlob, catCommitTree )

import Iolaus.Progress ( debugMessage )
import Iolaus.Flags ( Flag( Test, TestParents, NativeMerge, FirstParentMerge,
                            IolausSloppyMerge,
                            CauterizeAllHeads, CommutePast ) )
import Iolaus.FileName ( FileName, fp2fn )
import Iolaus.IO ( ExecutableBit(..) )
import Iolaus.SlurpDirectoryInternal
    ( Slurpy(..), SlurpyContents(..), empty_slurpy,
      slurpies_to_map, map_to_slurpies )
import Iolaus.Lock ( removeFileMayNotExist )
import Iolaus.Diff ( diff )
import Iolaus.Patch ( Named, Prim, commute, apply_to_slurpy, mergeNamed,
                      list_touched_files, infopatch )
import Iolaus.TouchesFiles ( look_touch )
import Iolaus.Ordered ( FL(..), (:>)(..), (+>+), unsafeCoerceP, mapFL_FL )
import Iolaus.Sealed ( Sealed(..), FlippedSeal(..), mapSeal, mapSealM, unseal )

#include "impossible.h"

touchedFiles :: IO [FilePath]
touchedFiles =
    do x <- lsothers
       y <- diffFiles [NameOnly] []
       return (x++lines y)

commitTouches :: Sealed (Hash Commit) -> IO [FilePath]
commitTouches (Sealed c) =
    lines `fmap` diffTreeCommit [NameOnly, DiffRecursive] c []

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
simplifyParents opts pars0 rec0 =
    do Sealed x <- mergeCommits opts (cauterizeHeads pars0)
                   >>= mapSealM slurpTree
       y <- slurpTree rec0
       simpHelp opts (cauterizeHeads pars0) (diff opts x y)

simpHelp :: [Flag] -> [Sealed (Hash Commit)] -> FL Prim C(w x)
         -> IO ([Sealed (Hash Commit)], Sealed (Hash Tree))
simpHelp opts pars0 pat0 = sp (cpnum opts) [] pars0 pat0
    where
      cpnum [] = 10000
      cpnum (CommutePast (-1):fs) = cpnum fs
      cpnum (CommutePast n:_) = n
      cpnum (CauterizeAllHeads:_) = 0
      cpnum (_:fs) = cpnum fs
      sp :: Int -> [Sealed (Hash Commit)] -> [Sealed (Hash Commit)]
         -> FL Prim C(w x) -> IO ([Sealed (Hash Commit)], Sealed (Hash Tree))
      sp _ kn [] patch =
          do Sealed ptree <- mergeCommits opts kn >>= mapSealM slurpTree
             debugMessage "In sp, no more parents..."
             t <- apply_to_slurpy (unsafeCoerceP patch) ptree >>= writeSlurpTree
             return (cauterizeHeads kn,Sealed t)
      sp 0 kn ps patch =
          do Sealed ptree <- mergeCommits opts (kn++ps) >>= mapSealM slurpTree
             debugMessage "In sp, we've tried enough times..."
             t <- apply_to_slurpy (unsafeCoerceP patch) ptree >>= writeSlurpTree
             return (cauterizeHeads (kn++ps),Sealed t)
      sp n kn (p:ps) patch =
          do let nop = cauterizeHeads (kn++ps++unseal parents p)
             tf <- commitTouches p
             if not (look_touch tf patch || null (unseal parents p))
                then -- FIXME: need to handle --test-parents here
                     do debugMessage "skipping an easy commit..."
                        debugMessage $ unlines $ list_touched_files patch
                        ok <-
                           if TestParents `elem` opts
                           then do Sealed x <- mapSealM catCommit p
                                   putStrLn $ "\n\nRunning test without:\n"++
                                            myMessage x
                                   Sealed noptree <- mergeCommits opts nop
                                                     >>= mapSealM slurpTree
                                   t' <- apply_to_slurpy (unsafeCoerceP patch)
                                                         noptree
                                         >>= writeSlurpTree
                                   testPredicate opts t'
                           else return True
                        if ok then sp (n-1) kn (ps++unseal parents p) patch
                              else sp (n-1) (p:kn) ps patch
                else
                  do Sealed ptree <- mergeCommits opts (kn++p:ps)
                                     >>= mapSealM slurpTree
                     debugMessage "In sp, trying with one less..."
                     t <- apply_to_slurpy (unsafeCoerceP patch) ptree
                          >>= writeSlurpTree
                     Sealed noptree <- mergeCommits opts nop
                                       >>= mapSealM slurpTree
                     case commute (diff opts noptree ptree :>
                                   unsafeCoerceP patch) of
                       Nothing -> sp (n-1) (p:kn) ps patch
                       Just (myp :> _) ->
                           do t' <- apply_to_slurpy myp noptree
                                    >>= writeSlurpTree
                              ct <- commitTree t
                                    (cauterizeHeads (p:kn++ps)) "iolaus:testing"
                              joined0 <- mergeCommitsX MergeN (Sealed ct:p:nop)
                              ct' <- commitTree t' nop "iolaus:testing"
                              joined <- mergeCommitsX MergeN (Sealed ct':p:nop)
                              ok <-
                                  if joined == joined0
                                  then
                                      if TestParents `elem` opts
                                      then do Sealed x <- mapSealM catCommit p
                                              putStrLn $
                                                "\n\nRunning test without:\n"++
                                                myMessage x
                                              testPredicate opts t'
                                      else return True
                                  else return False
                              if ok
                                then sp (n-1) kn (filter (`notElem` kn) nop) myp
                                else sp (n-1) (p:kn) ps patch

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
               let diffempty (Sealed x,Sealed ss) =
                      do msg <- (concat.take 1.lines.myMessage)
                                `fmap` catCommit x
                         return $ Sealed $ mapFL_FL (infopatch msg) $
                                diff [] empty_slurpy ss
               Sealed p <- mergeNamed `fmap` mapM diffempty (zip xs sls)
               Sealed `fmap` writeSlurpTree
                          (fromJust $ apply_to_slurpy p empty_slurpy)
      Sealed ancestor:_ ->
          do diffs <- newIORef M.empty
             pps <- mapM (mapSealM (bigDiff diffs s ancestor)) xs
             Sealed ps <- return $ mergeNamed pps
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

bigDiff :: IORef (M.Map (Sealed (Hash Commit))
                 (Sealed (FL (Named String Prim) C(x))))
        -> Strategy
        -> Hash Commit C(x) -> Hash Commit C(y)
        -> IO (FL (Named String Prim) C(x y))
bigDiff dags strat a0 me0 =
    maybe (error "bad bigDiff") (diffDag dags strat) $ makeDag a0 me0

diffDag :: IORef (M.Map (Sealed (Hash Commit))
                 (Sealed (FL (Named String Prim) C(x))))
        -> Strategy -> Dag C(x y) -> IO (FL (Named String Prim) C(x y))
diffDag _ _ (Ancestor _) = return NilFL
diffDag c s (Node x ys) =
    do m <- readIORef c
       case M.lookup (Sealed x) m of
         Just (Sealed ps) -> return $ unsafeCoerceP ps
         Nothing -> do ps <- diffDagHelper c s (Node x ys)
                       modifyIORef c (M.insert (Sealed x) (Sealed ps))
                       return ps

diffDagHelper :: IORef (M.Map (Sealed (Hash Commit))
                       (Sealed (FL (Named String Prim) C(x))))
              -> Strategy -> Dag C(x y) -> IO (FL (Named String Prim) C(x y))
diffDagHelper _ _ (Ancestor _) = return NilFL
diffDagHelper _ _ (Node _ []) = impossible
diffDagHelper _ _ (Node x [Sealed (Ancestor y)]) =
    do old <- catCommitTree y >>= slurpTree
       new <- catCommitTree x >>= slurpTree
       msg <- (concat . take 1 . lines . myMessage) `fmap` catCommit x
       return $ mapFL_FL (infopatch msg) $ diff [] old new
diffDagHelper c s (Node x [Sealed y@(Node yy _)]) =
    do old <- catCommitTree yy >>= slurpTree
       new <- catCommitTree x >>= slurpTree
       oldps <- diffDag c s y
       msg <- (concat . take 1 . lines . myMessage) `fmap` catCommit x
       return $ oldps +>+ mapFL_FL (infopatch msg) (diff [] old new)
diffDagHelper c MergeN (Node x ys) =
    do oldest <- catCommitTree (greatGrandFather (Node x ys)) >>= slurpTree
       new <- catCommitTree x >>= slurpTree
       tracks <- mapM (mapSealM (diffDag c MergeN)) ys
       Sealed oldps <- return $ mergeNamed tracks
       let Just old = apply_to_slurpy oldps oldest
       msg <- (concat . take 1 . lines . myMessage) `fmap` catCommit x
       return $ oldps +>+ mapFL_FL (infopatch msg) (diff [] old new)
diffDagHelper c s (Node x ys@(Sealed y0:_)) = -- this version is buggy!!!
    do Sealed merged <- mergeCommits0 s (map (mapSeal dag2commit) ys)
                        >>= mapSealM slurpTree
       ps0 <- diffDag c s y0
       old <- catCommitTree (dag2commit y0) >>= slurpTree
       new <- catCommitTree x >>= slurpTree
       msg <- (concat . take 1 . lines . myMessage) `fmap` catCommit x
       return $ ps0 +>+ mapFL_FL (infopatch "merge") (diff [] old merged)
                    +>+ mapFL_FL (infopatch msg) (diff [] merged new)

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
       c <- commitTree t (cauterizeHeads hs) "iolaus:temp"
       revList (show c) (Skip 1:revlistopts)
