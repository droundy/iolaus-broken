{-# LANGUAGE CPP #-}
#include "gadts.h"

module Git.Plumbing ( Hash, mkHash, Tree, Commit, Blob(Blob), Tag, emptyCommit,
                      catBlob, hashObject,
                      catTree, TreeEntry(..),
                      catCommit, CommitEntry(..),
                      catCommitTree, parseRev,
                      heads, remoteHeads, headNames, remoteHeadNames,
                      clone, gitInit, fetchPack, sendPack, listRemotes,
                      checkoutCopy,
                      lsfiles, lssomefiles, lsothers,
                      revList, revListHashes, RevListOption(..), nameRevs,
                      updateindex, updateIndexForceRemove, updateIndexCacheInfo,
                      writetree, mkTree, readTree, readTreeMerge, checkoutIndex,
                      updateref,
                      diffFiles, diffTrees, diffTreeCommit, DiffOption(..),
                      gitApply,
                      mergeBase, mergeIndex,
                      mergeFile, unpackFile,
                      getColor, getColorWithDefault,
                      headhash, commitTree ) where

import System.IO ( Handle, hGetContents, hPutStr, hClose )
-- import System.IO.Pipe ( openPipe )
import System.Exit ( ExitCode(..) )
import System.IO.Error ( isDoesNotExistError )
import System.Directory ( removeFile )
import Control.Exception ( catchJust, ioErrors )
#ifdef HAVE_REDIRECTS
import System.Process.Redirects ( createProcess, waitForProcess, proc,
                                  CreateProcess(..), StdStream(..) )
#else
import System.Process ( createProcess, waitForProcess, proc,
                        CreateProcess(..), StdStream(..) )
#endif
import qualified Data.ByteString as B
import Iolaus.FileName ( FileName, fp2fn, fn2fp )
import Iolaus.Global ( debugMessage )
import Iolaus.Sealed ( Sealed(Sealed) )
import Iolaus.Show ( Show1(..), Eq1(..), Ord1(..), Pretty1(..), Pretty(..) )

data Hash a C(x) = Hash !a !String
                   deriving ( Eq, Ord )
instance Show1 (Hash a) where show1 (Hash _ s) = s
instance Show (Hash a C(x)) where show = show1
instance Eq1 (Hash a) where
    eq1 (Hash _ x) (Hash _ y) = x == y
instance Ord1 (Hash a) where
    compare1 (Hash _ x) (Hash _ y) = compare x y

mkHash :: a -> String -> Hash a C(x)
mkHash a s = Hash a (cleanhash s)

emptyCommit :: Hash Commit C(())
emptyCommit = Hash Commit (take 40 $ repeat '0')

mkSHash :: a -> String -> Sealed (Hash a)
mkSHash a s = Sealed $ Hash a (cleanhash s)

data Tag = Tag deriving ( Show, Eq, Ord )
data Blob = Blob deriving ( Show, Eq, Ord )
data Tree = Tree deriving ( Show, Eq, Ord )
data Commit = Commit deriving ( Show, Eq, Ord )

readTree :: Hash Tree C(x) -> String -> IO ()
readTree t i =
    do removeFileMayNotExist (".git/"++i)
       debugMessage "calling git read-tree --index-output=..."
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ["read-tree","--index-output="++".git/"++i,
                                                show t])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git read-tree failed"

mergeBase :: Hash Commit C(x) -> Hash Commit C(y) -> IO (Sealed (Hash Commit))
mergeBase a b =
    do debugMessage "calling git merge-base"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["merge-base", show a, show b])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkSHash Commit out
         ExitFailure _ -> fail "git merge-base failed"

mergeIndex :: String -> IO (Sealed (Hash Tree))
mergeIndex i =
    do debugMessage ("calling git merge-index")
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ["merge-index", "git-imof", "-a"])
                         { env = Just [("GIT_INDEX_FILE",".git/"++i)] }
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git merge-index failed"
       debugMessage "calling git write-tree"
       (Nothing, Just stdout, Nothing, pid2) <-
           createProcess (proc "git" ["write-tree"])
                             { std_out = CreatePipe,
                               env = Just [("GIT_INDEX_FILE",".git/"++i)] }
       out <- hGetContents stdout
       ec2 <- length out `seq` waitForProcess pid2
       case ec2 of
         ExitSuccess -> return $ mkSHash Tree out
         ExitFailure _ -> fail "git write-tree failed in mergeIndex"

readTreeMerge :: Hash Tree C(x) -> Hash Tree C(y) -> Hash Tree C(z)
              -> String -> IO ()
readTreeMerge o a b i =
    do removeFileMayNotExist (".git/"++i)
       let args = ["--index-output=.git/"++i, "-m","-i",
                   show o, show a, show b]
       debugMessage ("calling git read-tree "++unwords args)
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ("read-tree":args))
                         { env = Just [("GIT_INDEX_FILE",".git/"++i)] }
       ec2 <- waitForProcess pid
       case ec2 of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git read-tree -m failed"

checkoutIndex :: FilePath -> FilePath -> IO ()
checkoutIndex i pfx =
    do debugMessage ("calling git checkout-index -a --prefix="++pfx)
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ["checkout-index","-a","--prefix="++pfx])
                         { env = Just [("GIT_INDEX_FILE",".git/"++i)] }
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git checkout-index failed"

checkoutCopy :: String -> IO ()
checkoutCopy pfx =
    do debugMessage "calling git checkout-index -a --prefix=..."
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ["checkout-index","-a","--prefix="++pfx])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git checkout-index failed"

lsfiles :: IO [String]
lsfiles =
    do debugMessage "calling git ls-files"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["ls-files","--exclude-standard",
                                               "--others","--cached"])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ lines out
         ExitFailure _ -> fail "git ls-files failed"

lssomefiles :: [String] -> IO [String]
lssomefiles [] = return []
lssomefiles fs =
    do debugMessage "calling git ls-files"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ("ls-files":"--":fs))
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ lines out
         ExitFailure _ -> fail "git ls-files failed"

lsothers :: IO [String]
lsothers =
    do debugMessage "calling git ls-files --others"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["ls-files","--others",
                                      "--exclude-standard"])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ lines out
         ExitFailure _ -> fail "git ls-files failed"

class Flag a where
    toFlags :: a -> [String]

data DiffOption = DiffAll | Stat | DiffPatch | NameOnly | DiffRaw
                | DiffRecursive
instance Flag DiffOption where
    toFlags DiffAll = ["-a"]
    toFlags Stat = ["--stat"]
    toFlags DiffPatch = ["-p"]
    toFlags DiffRaw = ["--raw"]
    toFlags NameOnly = ["--name-only"]
    toFlags DiffRecursive = ["-r"]

diffFiles :: [DiffOption] -> [FilePath] -> IO String
diffFiles opts fs =
    do let flags = case opts of [] -> ["-p"]
                                _ -> concatMap toFlags opts
       debugMessage "calling git diff-files"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ("diff-files":flags++"--":fs))
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git diff-files failed"

diffTrees :: [DiffOption] -> Hash Tree C(x) -> Hash Tree C(y)
          -> [FilePath] -> IO String
diffTrees opts t1 t2 fs =
    do let flags = case opts of [] -> ["-p"]
                                _ -> concatMap toFlags opts
           allflags = flags++show t1:show t2:"--":fs
       debugMessage ("calling git diff-tree "++show allflags)
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ("diff-tree":allflags))
                             {std_out = CreatePipe}
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git diff-tree failed"

diffTreeCommit :: [DiffOption] -> Hash Commit C(x) -> [FilePath] -> IO String
diffTreeCommit opts c fs =
    do let flags = case opts of [] -> ["-p"]
                                _ -> concatMap toFlags opts
           allflags = flags++show c:"--":fs
       debugMessage ("calling git diff-tree -c "++show allflags)
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ("diff-tree":"-c":allflags))
                             {std_out = CreatePipe}
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git diff-tree failed"

headhash :: IO (Sealed (Hash Commit))
headhash =
    do debugMessage "calling git show-ref -h"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["show-ref","-h"]) { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkSHash Commit out
         ExitFailure _ -> fail "git show-ref failed"

updateIndexForceRemove :: FilePath -> IO ()
updateIndexForceRemove fp =
    do debugMessage "calling git update-index --force-remove"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ["update-index","--force-remove","--",fp])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git update-index failed"

updateIndexCacheInfo :: String -> Hash Blob C(x) -> FilePath -> IO ()
updateIndexCacheInfo mode sha fp =
    do debugMessage "calling git update-index"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ["update-index",
                                      "--cacheinfo",mode,show sha,fp])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git update-index failed"

mergeFile :: FilePath -> FilePath -> FilePath -> IO (Hash Blob C(x))
mergeFile s1 a s2 =
    do debugMessage "calling git merge-file"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["merge-file","-L","mine","-L","ancestor",
                                                 "-L","yours","--stdout",
                                                 s1,a,s2])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitFailure e | e < 0 -> fail "git merge-file failed"
         _ -> hashObject (`hPutStr` out)

unpackFile :: Hash Blob C(x) -> IO FilePath
unpackFile sha =
    do debugMessage "calling git unpack-file"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["unpack-file",show sha])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ init out
         ExitFailure _ -> fail "git unpack-file failed"

updateindex :: [String] -> IO ()
updateindex [] = debugMessage "no need to call git update-index"
updateindex fs =
    do debugMessage "calling git update-index"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ("update-index":
                                      "--add":"--remove":"--":fs))
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git update-index failed"

writetree :: IO (Sealed (Hash Tree))
writetree = 
    do debugMessage "calling git write-tree"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["write-tree"]) { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkSHash Tree out
         ExitFailure _ -> fail "git write-tree failed"

heads :: IO [Sealed (Hash Commit)]
heads =
    do debugMessage "calling git rev-parse"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["rev-parse", "--branches"])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ map (mkSHash Commit) $ lines out
         ExitFailure _ -> fail "parseRev failed"

remoteHeads :: String -> IO [Sealed (Hash Commit)]
remoteHeads repo =
    do debugMessage "calling git show-ref"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["ls-remote", "--heads",repo])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ map (mkSHash Commit) $ lines out
         ExitFailure _ -> fail "parseRev failed"

headNames :: IO [(Sealed (Hash Commit), String)]
headNames =
    do debugMessage "calling git show-ref"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["show-ref", "--heads"])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ map parse $ lines out
         ExitFailure _ -> fail "git show-ref failed"
    where parse l = (mkSHash Commit l, drop 41 l)

remoteHeadNames :: String -> IO [(Sealed (Hash Commit), String)]
remoteHeadNames repo =
    do debugMessage "calling git ls-remote"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["ls-remote", "--heads",repo])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ map parse $ lines out
         ExitFailure _ -> fail "git ls-remote failed"
    where parse l = (mkSHash Commit l, drop 41 l)

parseRev :: String -> IO (Sealed (Hash Commit))
parseRev s =
    do debugMessage "calling git rev-parse"
       (Nothing, Just stdout, Just stderr, pid) <-
           createProcess (proc "git" ["rev-parse", "--verify",s])
#ifdef HAVE_REDIRECTS
                   { std_err = Just CreatePipe, std_out = CreatePipe }
#else
                   { std_err = CreatePipe, std_out = CreatePipe }
#endif
       out <- hGetContents stdout
       err <- hGetContents stderr
       ec <- length (out++err) `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkSHash Commit out
         ExitFailure _ -> fail ("git rev-parse failed: "++err)

updateref :: String -> Hash Commit C(x) -> IO ()
updateref r h =
    do debugMessage "calling git update-ref"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ["update-ref",r,show h])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git update-ref failed"

commitTree :: Hash Tree C(x) -> [Sealed (Hash Commit)] -> String
           -> IO (Hash Commit C(x))
commitTree t pars m =
    do let pflags = concatMap (\p -> ["-p",show p]) pars
       debugMessage "calling git commit-tree"
       (Just i, Just o, Nothing, pid) <-
           createProcess (proc "git" ("commit-tree":show t:pflags)) {
                               std_out = CreatePipe,
                               std_in = CreatePipe }
       out <- hGetContents o
       hPutStr i m
       hClose i
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkHash Commit out
         ExitFailure _ -> fail "git commit-tree failed"

cleanhash :: String -> String
cleanhash = take 40

data RevListOption = MediumPretty | OneLine | Authors
                   | Graph | RelativeDate | MaxCount Int | Skip Int
instance Flag RevListOption where
    toFlags MediumPretty = ["--pretty=medium"]
    toFlags OneLine = ["--pretty=oneline"]
    toFlags Authors = ["--pretty=format:%an"]
    toFlags Graph = ["--graph"]
    toFlags RelativeDate = ["--date=relative"]
    toFlags (MaxCount n) = ["--max-count="++show n]
    toFlags (Skip n) = ["--skip="++show n]

nameRevs :: IO [String]
nameRevs =
    do debugMessage "calling git name-rev"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["name-rev", "--all"])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ concatMap prett $ lines out
         ExitFailure _ -> fail "git rev-list failed"
    where prett s = case words s of
                       [_,"undefined"] -> []
                       [sha,n] | '~' `elem` n -> [sha]
                               | otherwise -> [sha,n]
                       _ -> error "bad stuff in nameRevs"

revList :: String -> [RevListOption] -> IO String
revList version opts =
    do let flags = concatMap toFlags opts
       debugMessage "calling git rev-list"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ("rev-list":version:flags))
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git rev-list failed"

revListHashes :: IO [Sealed (Hash Commit)]
revListHashes = do x <- revList "master" []
                   return $ map (mkSHash Commit) $ words x

-- | FIXME: I believe that clone is porcelain...

clone :: [String] -> IO ()
clone args =
    do debugMessage "calling git clone"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ("clone":args))
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git clone failed"

-- | FIXME: I believe that init is porcelain...

gitInit :: [String] -> IO ()
gitInit args =
    do debugMessage "calling git init"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ("init":args))
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git init failed"

catBlob :: Hash Blob C(x) -> IO B.ByteString
catBlob (Hash Blob h) =
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["cat-file","blob",h])
                             { std_out = CreatePipe }
       out <- B.hGetContents stdout
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git cat-file blob failed"

data TreeEntry C(x) = Subtree (Hash Tree C(x))
                    | File (Hash Blob C(x))
                    | Executable (Hash Blob C(x))
                    | Symlink (Hash Blob C(x))
instance Show1 TreeEntry where
    show1 (Subtree (Hash Tree h)) = "040000 tree "++h
    show1 (File (Hash Blob h)) = "100644 blob "++h
    show1 (Executable (Hash Blob h)) = "100755 blob "++h
    show1 (Symlink (Hash Blob h)) = "120000 blob "++h
instance Show (TreeEntry C(x)) where show = show1

catTree :: Hash Tree C(x) -> IO [(FileName, TreeEntry C(x))]
catTree (Hash Tree h) =
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["cat-file","-p",h])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> mapM parseit $ lines out
         ExitFailure _ -> fail "git cat-file -p failed"
    where parseit x =
              case splitAt 12 x of
                ("040000 tree ",x') ->
                    case splitAt 40 x' of
                      (z, _:fp) -> return (fp2fn fp, Subtree $ Hash Tree z)
                      (_,[]) -> fail "error tree"
                ("100755 blob ",x') ->
                    case splitAt 40 x' of
                      (z, _:fp) -> return (fp2fn fp, Executable $ Hash Blob z)
                      (_,[]) -> fail "error blob exec"
                ("100644 blob ",x') ->
                    case splitAt 40 x' of
                      (z, _:fp) -> return (fp2fn fp, File $ Hash Blob z)
                      (_,[]) -> fail "error blob"
                ("120000 blob ",x') ->
                    case splitAt 40 x' of
                      (z, _:fp) -> return (fp2fn fp, Symlink $ Hash Blob z)
                      (_,[]) -> fail "error blob exec"
                _ -> fail "weird line in tree"

catCommitTree :: Hash Commit C(x) -> IO (Hash Tree C(x))
catCommitTree c = myTree `fmap` catCommit c

data CommitEntry C(x) = CommitEntry { myParents :: [Sealed (Hash Commit)],
                                      myTree :: Hash Tree C(x),
                                      myAuthor :: String,
                                      myCommitter :: String,
                                      myMessage :: String }

instance Show1 CommitEntry where
    show1 c | myAuthor c == myCommitter c =
                unlines $ ("tree "++show (myTree c)) :
                        map (\p -> "parent "++show p) (myParents c)
                ++ ["author "++myAuthor c,"", myMessage c]
    show1 c = unlines $ ("tree "++show (myTree c)) :
             map (\p -> "parent "++show p) (myParents c)
             ++ ["author "++myAuthor c, "committer "++myCommitter c,
                 "", myMessage c]
instance Show (CommitEntry C(x)) where
    show = show1

instance Pretty1 CommitEntry where
    pretty1 c | myAuthor c == myCommitter c =
                  myAuthor c++"\n  * "++n++"\n"++unlines (map ("    "++) cl)
              where n:cl = lines $ myMessage c
    pretty1 c = myAuthor c++"\n"++myCommitter c++
                "\n  * "++n++"\n"++unlines (map ("    "++) cl)
              where n:cl = lines $ myMessage c
instance Pretty (CommitEntry C(x)) where
    pretty = pretty1

catCommit :: Hash Commit C(x) -> IO (CommitEntry C(x))
catCommit (Hash Commit h0) =
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["cat-file","commit",h0])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> parseit $ lines out
         ExitFailure _ -> fail "git cat-file blob failed"
    where parseit (x:xs) =
              case words x of
              [] -> return $ CommitEntry { myParents = [],
                                           myAuthor = "",
                                           myCommitter = "",
                                           myTree = error "xx234",
                                           myMessage = unlines xs }
              ["tree",h] -> do c <- parseit xs
                               return $ c { myTree = mkHash Tree h }
              ["parent",h] ->
                  do c <- parseit xs
                     return $ c { myParents = mkSHash Commit h : myParents c }
              "author":_ ->
                  do c <- parseit xs
                     return $ c { myAuthor = drop 7 x }
              "committer":_ ->
                  do c <- parseit xs
                     return $ c { myCommitter = drop 10 x }
              _ -> fail "weird stuff in commitTree"
          parseit [] = fail "empty commit in commitTree?"

hashObject :: (Handle -> IO ()) -> IO (Hash Blob C(x))
hashObject wr =
    do (Just i, Just o, Nothing, pid) <-
           createProcess (proc "git" ["hash-object","-w","--stdin"])
                             { std_in = CreatePipe,
                               std_out = CreatePipe }
       out <- hGetContents o
       wr i
       hClose i
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkHash Blob out
         ExitFailure _ -> fail ("git hash-object failed\n"++out)

mkTree :: [(FileName, TreeEntry C(x))] -> IO (Hash Tree C(x))
mkTree xs =
    do debugMessage "calling git mk-tree"
       (Just i, Just o, Nothing, pid) <-
           createProcess (proc "git" ["mktree"])
                             { std_in = CreatePipe,
                               std_out = CreatePipe }
       out <- hGetContents o
       mapM_ (putStuff i) xs
       hClose i
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkHash Tree out
         ExitFailure _ -> fail "git mk-tree failed"
    where putStuff i (f, te) = hPutStr i (show te++'\t':fn2fp f++"\n")

gitApply :: FilePath -> IO ()
gitApply p =
    do debugMessage "calling git apply"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ["apply", p])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git apply failed"

listConfig :: IO [(String, String)]
listConfig =
    do debugMessage "calling git config"
       (Nothing, Just o, Nothing, pid) <-
           createProcess (proc "git" ["config", "--null", "--list"])
                         { std_out = CreatePipe }
       out <- hGetContents o
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ parse out
         ExitFailure _ -> fail "git config failed"
    where parse "" = []
          parse x = case break (== '\n') x of
                      (a,_:b) ->
                          case break (== '\0') b of
                            (c,_:d) -> (a,c) : parse d
                            (c,"") -> [(a,c)]
                      _ -> []

remoteUrls :: IO [(String,String)]
remoteUrls = concatMap getrepo `fmap` listConfig
    where getrepo (x,y) =
              if take 7 x == "remote." && take 4 (reverse x) == "lru."
              then [(reverse $ drop 4 $ reverse $ drop 7 x, y)]
              else []

listRemotes :: IO [String]
listRemotes = map fst `fmap` remoteUrls

parseRemote :: String -> IO String
parseRemote r = do xs <- remoteUrls
                   case lookup r xs of
                     Just u -> return u
                     Nothing -> return r

fetchPack :: String -> IO ()
fetchPack repo0 =
    do repo <- parseRemote repo0
       debugMessage ("calling git fetch-pack --all "++repo)
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ["fetch-pack", "--all", repo])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git fetch-pack failed"

sendPack :: String -> [(Sealed (Hash Commit), String)] -> IO ()
sendPack repo0 xs =
    do repo <- parseRemote repo0
       debugMessage ("calling git send-pack --force "++repo)
       let revs = map (\ (h,r) -> show h++':':r) xs
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ("send-pack":"--force":repo:revs))
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git send-pack failed"

getColor :: String -> IO String
getColor c =
    do (Nothing, Just o, Nothing, pid) <-
           createProcess (proc "git" ["config", "--get-color", c])
                         { std_out = CreatePipe }
       out <- hGetContents o
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git config failed"

getColorWithDefault :: String -> String -> IO String
getColorWithDefault c d =
    do (Nothing, Just o, Nothing, pid) <-
           createProcess (proc "git" ["config", "--get-color", c, d])
                         { std_out = CreatePipe }
       out <- hGetContents o
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git config failed"


removeFileMayNotExist :: String -> IO ()
removeFileMayNotExist f =
    catchJust ioErrors (removeFile f) $ \e ->
        if isDoesNotExistError e then return ()
                                 else ioError e
