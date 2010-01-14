{-# LANGUAGE CPP #-}
#include "gadts.h"

module Git.Plumbing ( Hash, mkHash, Tree, Commit, Blob(Blob), Tag, emptyCommit,
                      catBlob, hashObject, committer, uname,
                      catTree, TreeEntry(..),
                      catCommit, catCommitRaw, CommitEntry(..),
                      catCommitTree, parseRev, maybeParseRev,
                      heads, remoteHeads, quickRemoteHeads, headNames, tagNames,
                      remoteHeadNames, remoteTagNames,
                      remoteAdd, gitInit, sendPack, listRemotes,
                      checkoutCopy,
                      lsfiles, lssomefiles, lsothers,
                      revList, revListHashes, RevListOption(..), nameRevs,
                      formatRev,
                      updateindex, updateIndexForceRemove, updateIndexCacheInfo,
                      writetree, mkTree, readTree, checkoutIndex,
                      updateref,
                      diffFiles, diffTrees, diffTreeCommit, DiffOption(..),
                      gitApply,
                      unpackFile,
                      getColor, getColorWithDefault,
                      getAllConfig, getConfig, setConfig, unsetConfig,
                      ConfigOption(..),
                      commitTree ) where

import System.IO ( Handle, hGetContents, hPutStr, hClose )
-- import System.IO.Pipe ( openPipe )
import System.Exit ( ExitCode(..) )
import System.IO.Error ( isDoesNotExistError )
import System.Directory ( removeFile )
import Control.Exception ( catchJust, ioErrors )
import Control.Monad ( unless, when )
import Control.Concurrent ( forkIO )
import Data.List ( isInfixOf )
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
    do debugMessage ("calling git checkout-index -a --prefix="++pfx)
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
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["rev-parse", "--branches"])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ map (mkSHash Commit) $ lines out
         ExitFailure _ -> fail "parseRev failed"

-- | like `remoteHeads`, but tries to avoid using the network if
-- possible.
quickRemoteHeads :: String -> IO [Sealed (Hash Commit)]
quickRemoteHeads repo = map fst `fmap` quickRemoteHeadNames repo

quickRemoteHeadNames :: String -> IO [(Sealed (Hash Commit), String)]
quickRemoteHeadNames repo =
    do rhns <- sloppyRemoteHeadNames repo
       case rhns of
         [] -> remoteHeadNames repo
         hs -> do -- There is a danger is that if we never pull
                  -- or push from repo, then the
                  -- refs/remotes/repo/master* might never be
                  -- updated.  This forkIO addresses that.
                  forkIO $ fetchRemote repo `catch` \_ -> return ()
                  return hs

sloppyRemoteHeadNames :: String -> IO [(Sealed (Hash Commit), String)]
sloppyRemoteHeadNames repo =
    do debugMessage "calling git show-ref"
       reporef <- remoteRemote repo
       let parse l
               | (reporef++"master") `isInfixOf` l =
                   [(mkSHash Commit l,
                     "refs/heads/"++reverse (takeWhile (/= '/') $ reverse l))]
           parse _ = []
                     
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["show-ref"])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitFailure _ -> return []
         ExitSuccess -> return $ concatMap parse $ lines out

remoteHeads :: String -> IO [Sealed (Hash Commit)]
remoteHeads repo = map fst `fmap` remoteHeadNames repo

headNames :: IO [(Sealed (Hash Commit), String)]
headNames =
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["show-ref", "--heads"])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ filter ismaster $ map parse $ lines out
         ExitFailure _ -> fail "git show-ref failed"
    where parse l = (mkSHash Commit l, drop 41 l)
          ismaster = ("master" `isInfixOf`) . snd

tagNames :: IO [(Sealed (Hash Tag), String)]
tagNames =
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["show-ref", "--tags"])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ map parse $ lines out
         ExitFailure _ -> return [] -- show-ref fails if there are no tags
    where parse l = (mkSHash Tag l, drop 41 l)

remoteHeadNames :: String -> IO [(Sealed (Hash Commit), String)]
remoteHeadNames repo = do fetchRemote repo
                          sloppyRemoteHeadNames repo

rawRemoteHeadNames :: String -> IO [(Sealed (Hash Commit), String)]
rawRemoteHeadNames repo =
    do debugMessage (unwords ["calling git ls-remote","--heads",repo])
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["ls-remote", "--heads",repo])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitFailure _ -> return [] -- failure may mean an empty repository
         ExitSuccess -> return $ filter ismaster $ map parse $ lines out
    where parse l = (mkSHash Commit l, drop 41 l)
          ismaster = ("master" `isInfixOf`) . snd

remoteTagNames :: String -> IO [(Sealed (Hash Tag), String)]
remoteTagNames repo =
    do debugMessage "calling git ls-remote"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["ls-remote", "--tags",repo])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitFailure _ -> return []
         ExitSuccess -> do let ts = map parse $ filter ('^' `notElem`) $
                                    lines out
                           unless (null ts) $ fetchRemote repo
                           return ts
    where parse l = (mkSHash Tag l, drop 41 l)

parseRev :: String -> IO (Sealed (Hash Commit))
parseRev s =
    do (Nothing, Just stdout, Just stderr, pid) <-
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

maybeParseRev :: String -> IO (Maybe (Sealed (Hash Commit)))
maybeParseRev s =
    do (Nothing, Just stdout, Just stderr, pid) <-
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
         ExitSuccess -> return $ Just $ mkSHash Commit out
         ExitFailure _ -> return Nothing

updateref :: String -> Sealed (Hash Commit)
          -> Maybe (Sealed (Hash Commit)) -> IO ()
updateref r (Sealed (Hash Commit "0000000000000000000000000000000000000000"))
            (Just old)=
    do debugMessage $ unwords ["calling git update-ref -d",r,show old]
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ["update-ref","-d",r,show old])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git update-ref failed"
updateref r new mold =
    do let old = maybe (take 40 $ repeat '0') show mold
       debugMessage $ unwords $ "calling git update-ref -d":r:show new:[old]
       (Nothing, Nothing, Just e, pid) <-
           createProcess (proc "git" ["update-ref",r,show new,old])
#ifdef HAVE_REDIRECTS
                   { std_err = Just CreatePipe }
#else
                   { std_err = CreatePipe }
#endif
       err <- hGetContents e
       ec <- length err `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail ("git update-ref failed: "++err)

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

data RevListOption = MediumPretty | OneLine | Authors | TopoOrder
                   | Graph | RelativeDate | MaxCount Int | Skip Int
instance Flag RevListOption where
    toFlags MediumPretty = ["--pretty=medium"]
    toFlags OneLine = ["--pretty=oneline"]
    toFlags Authors = ["--pretty=format:%an"]
    toFlags Graph = ["--graph"]
    toFlags RelativeDate = ["--date=relative"]
    toFlags (MaxCount n) = ["--max-count="++show n]
    toFlags (Skip n) = ["--skip="++show n]
    toFlags TopoOrder = ["--topo-order"]

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

formatRev :: String -> Hash Commit C(x) -> IO String
formatRev fmt c =
    do debugMessage $ unwords ["calling git rev-list --pretty=format:",fmt,
                               show c]
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["rev-list","--max-count=1",
                                      "--pretty=format:"++fmt,show c])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ reverse $ dropWhile (=='\n') $ reverse $
                                 drop 1 $ dropWhile (/='\n') out
         ExitFailure _ -> fail "git rev-list failed"

revList :: [String] -> [RevListOption] -> IO String
revList version opts =
    do let flags = concatMap toFlags opts
       debugMessage "calling git rev-list"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ("rev-list":flags++version))
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git rev-list failed"

revListHashes :: [Sealed (Hash Commit)] -> [RevListOption]
              -> IO [Sealed (Hash Commit)]
revListHashes version opts =
    do x <- revList (map show version) opts
       return $ map (mkSHash Commit) $ words x

remoteAdd :: String -> String -> IO ()
remoteAdd rname url =
    do debugMessage "calling git remote add"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ["remote","add",rname,url])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git remote add failed"

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

catCommitRaw :: Hash Commit C(x) -> IO String
catCommitRaw (Hash Commit h0) =
    do (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git" ["cat-file","commit",h0])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git cat-file blob failed"

catCommit :: Hash Commit C(x) -> IO (CommitEntry C(x))
catCommit h0 = catCommitRaw h0 >>= (parseit . lines)
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

getAllConfig :: String -> IO [String]
getAllConfig v =
    do debugMessage "calling git config"
       (Nothing, Just o, Nothing, pid) <-
           createProcess (proc "git" ["config", "--null", "--get-all",v])
                         { std_out = CreatePipe }
       out <- hGetContents o
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ vals out
         ExitFailure _ -> fail "git config failed"
    where vals :: String -> [String]
          vals "" =  []
          vals s = case break (== '\0') s of (l, []) -> [l]
                                             (l, _:s') -> l : vals s'

data ConfigOption = Global | System | RepositoryOnly
instance Flag ConfigOption where
    toFlags Global = ["--global"]
    toFlags System = ["--system"]
    toFlags RepositoryOnly = ["--file",".git/config"]

getConfig :: [ConfigOption] -> String -> IO (Maybe String)
getConfig fs v =
    do debugMessage "calling git config"
       (Nothing, Just o, Nothing, pid) <-
           createProcess (proc "git" ("config":"--null":concatMap toFlags fs
                                      ++["--get",v]))
                         { std_out = CreatePipe }
       out <- hGetContents o
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ Just $ takeWhile (/= '\0') out
         ExitFailure _ -> return Nothing

setConfig :: [ConfigOption] -> String -> String -> IO ()
setConfig fs c v =
    do debugMessage "calling git config"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ("config":concatMap toFlags fs++[c, v]))
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git config failed"

unsetConfig :: [ConfigOption] -> String -> IO ()
unsetConfig fs c =
    do debugMessage "calling git config"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ("config":concatMap toFlags fs++
                                      ["--unset",c]))
       waitForProcess pid
       return ()

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

fetchRemote :: String -> IO ()
fetchRemote repo =
    do debugMessage "am in fetchRemote"
       nhs <- map sw `fmap` rawRemoteHeadNames repo
       nhs0 <- map sw `fmap` sloppyRemoteHeadNames repo
       when (nhs /= nhs0) $
            do debugMessage "need to actually update the remote..."
               fetchPack repo
               r <- remoteRemote repo
               -- the following "drop 11" cuts off the string
               -- "refs/heads/" so we can replace it with
               -- "refs/remotes/remotename/".
               let upd (n,h) = updateref (r++drop 11 n) h (lookup n nhs0)
               mapM_ upd nhs
               let upd2 (n,h) = case lookup n nhs of
                                  Nothing -> updateref (r++drop 11 n)
                                             (Sealed emptyCommit) (Just h)
                                  Just _ -> return ()
               mapM_ upd2 nhs0
    where sw (a,b) = (b,a)

remoteRemote :: String -> IO String
remoteRemote repo0 =
    do repo <- parseRemote repo0
       if repo /= repo0
          then return ("refs/remotes/"++repo0++"/")
          else return $ "refs/remotes/"++concatMap cleanup repo0++"/"
    where cleanup '/' = "-"
          cleanup ':' = "_"
          cleanup '.' = "-"
          cleanup '\\' = "-"
          cleanup '@' = "_"
          cleanup c = [c]

fetchPack :: String -> IO ()
fetchPack repo0 =
    do repo <- parseRemote repo0
       debugMessage ("calling git fetch-pack --all "++repo)
       (Nothing, Just o, Nothing, pid) <-
           createProcess (proc "git" ["fetch-pack", "--all", repo])
                         { std_out = CreatePipe }
       out <- hGetContents o
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git fetch-pack failed"

sendPack :: String -> [(Sealed (Hash Commit), String)] -> [String] -> IO ()
sendPack repo0 xs ts =
    do repo <- parseRemote repo0
       debugMessage ("calling git send-pack --force "++repo)
       let revs = map (\ (h,r) -> show h++':':r) xs
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git" ("send-pack":"--force":repo:revs++ts))
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> fetchRemote repo0
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

committer :: IO String
committer =
    do (Nothing, Just o, Nothing, pid) <-
           createProcess (proc "git" ["var", "GIT_COMMITTER_IDENT"])
                         { std_out = CreatePipe }
       out <- hGetContents o
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return (takeWhile (/= '>') out++">")
         ExitFailure _ -> fail "git var failed"

uname :: IO String
uname = do (Nothing, Just o, Nothing, pid) <-
               createProcess (proc "uname" ["-n", "-m", "-o"])
                                 { std_out = CreatePipe }
           out <- hGetContents o
           ec <- length out `seq` waitForProcess pid
           case ec of
             ExitSuccess -> return $ filter (/= '\n') out
             ExitFailure _ -> fail "uname failed"
