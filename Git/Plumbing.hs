module Git.Plumbing ( Hash, Tree, Commit, Blob, Tag,
                      catBlob, hashObject,
                      catTree, TreeEntry(..),
                      catCommitTree, parseRev,
                      clone,
                      checkoutCopy,
                      lsfiles, lssomefiles, lsothers,
                      revList, revListHashes, RevListOption(..),
                      updateindex,
                      writetree, mkTree, readTree, checkoutIndex,
                      updateref,
                      diffFiles, diffTrees, DiffOption(..), gitApply,
                      headhash, commitTree ) where

import System.IO ( Handle, hGetContents, hPutStr, hClose )
-- import System.IO.Pipe ( openPipe )
import System.Exit ( ExitCode(..) )
import System.Process.Redirects ( createProcess, waitForProcess, proc,
                                  CreateProcess(..), StdStream(..) )
import qualified Data.ByteString as B
import Arcs.FileName ( FileName, fp2fn, fn2fp )
import Arcs.Progress ( debugMessage )
import Arcs.Lock ( removeFileMayNotExist )

data Hash a = Hash !a !String
              deriving ( Eq )
instance Show (Hash a) where
    show (Hash _ s) = s
mkHash :: a -> String -> Hash a
mkHash a s = Hash a (cleanhash s)

data Tag = Tag deriving ( Show, Eq, Ord )
data Blob = Blob deriving ( Show, Eq, Ord )
data Tree = Tree deriving ( Show, Eq, Ord )
data Commit = Commit deriving ( Show, Eq, Ord )

readTree :: Hash Tree -> String -> IO ()
readTree t i =
    do removeFileMayNotExist (".git/"++i)
       debugMessage "calling git-read-tree --index-output=..."
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git-read-tree" ["--index-output="++".git/"++i,
                                                show t])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git-read-tree failed"

checkoutIndex :: FilePath -> FilePath -> IO ()
checkoutIndex i pfx =
    do debugMessage ("calling git-checkout-index -a --prefix="++pfx)
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git-checkout-index" ["-a","--prefix="++pfx])
                         { env = Just [("GIT_INDEX_FILE",".git/"++i)] }
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git-checkout-index failed"

checkoutCopy :: String -> IO ()
checkoutCopy pfx =
    do debugMessage "calling git-checkout-index -a --prefix=..."
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git-checkout-index" ["-a","--prefix="++pfx])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git-checkout-index failed"

lsfiles :: IO [String]
lsfiles =
    do debugMessage "calling git-ls-files"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-ls-files" ["--exclude-standard",
                                               "--others","--cached"])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ lines out
         ExitFailure _ -> fail "git-ls-files failed"

lssomefiles :: [String] -> IO [String]
lssomefiles [] = return []
lssomefiles fs =
    do debugMessage "calling git-ls-files"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-ls-files" ("--":fs))
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ lines out
         ExitFailure _ -> fail "git-ls-files failed"

lsothers :: IO [String]
lsothers =
    do debugMessage "calling git-ls-files --others"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-ls-files" ["--others","--exclude-standard"])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ lines out
         ExitFailure _ -> fail "git-ls-files failed"

class Flag a where
    toFlags :: a -> [String]

data DiffOption = DiffAll | Stat | DiffPatch | NameOnly
instance Flag DiffOption where
    toFlags DiffAll = ["-a"]
    toFlags Stat = ["--stat"]
    toFlags DiffPatch = ["-p"]
    toFlags NameOnly = ["--name-only"]

diffFiles :: [DiffOption] -> [FilePath] -> IO String
diffFiles opts fs =
    do let flags = case opts of [] -> ["-p"]
                                _ -> concatMap toFlags opts
       debugMessage "calling git-diff-files"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-diff-files" (flags++"--":fs))
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git-diff-files failed"

diffTrees :: [DiffOption] -> Hash Tree -> Hash Tree -> [FilePath] -> IO String
diffTrees opts t1 t2 fs =
    do let flags = case opts of [] -> ["-p"]
                                _ -> concatMap toFlags opts
           allflags = flags++show t1:show t2:"--":fs
       debugMessage ("calling git-diff-tree "++show allflags)
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-diff-tree" allflags) {std_out = CreatePipe}
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git-diff-tree failed"

headhash :: IO (Hash Commit)
headhash =
    do debugMessage "calling git-show-ref -h"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-show-ref" ["-h"]) { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkHash Commit out
         ExitFailure _ -> fail "git-show-ref failed"

updateindex :: [String] -> IO ()
updateindex [] = debugMessage "no need to call git-update-index"
updateindex fs =
    do debugMessage "calling git-update-index"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git-update-index" ("--add":"--remove":"--":fs))
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git-update-index failed"

writetree :: IO (Hash Tree)
writetree = 
    do debugMessage "calling git-write-tree"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-write-tree" []) { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkHash Tree out
         ExitFailure _ -> fail "git-write-tree failed"

parseRev :: String -> IO (Hash Commit)
parseRev s =
    do debugMessage "calling git-rev-parse"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-rev-parse" ["--verify",s])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkHash Commit out
         ExitFailure _ -> fail "parseRev failed"

updateref :: String -> Hash Commit -> IO ()
updateref r h =
    do debugMessage "calling git-update-ref"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git-update-ref" [r,show h])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git-update-ref failed"

commitTree :: Hash Tree -> [Hash Commit] -> String -> IO (Hash Commit)
commitTree t pars m =
    do let pflags = concatMap (\p -> ["-p",show p]) pars
       debugMessage "calling git-commit-tree"
       (Just i, Just o, Nothing, pid) <-
           createProcess (proc "git-commit-tree" (show t:pflags)) {
                               std_out = CreatePipe,
                               std_in = CreatePipe }
       out <- hGetContents o
       hPutStr i m
       hClose i
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkHash Commit out
         ExitFailure _ -> fail "git-commit-tree failed"

cleanhash :: String -> String
cleanhash = take 40

data RevListOption = MediumPretty | OneLine | Authors
                   | Graph | RelativeDate | MaxCount Int
instance Flag RevListOption where
    toFlags MediumPretty = ["--pretty=medium"]
    toFlags OneLine = ["--pretty=oneline"]
    toFlags Authors = ["--pretty=format:%an"]
    toFlags Graph = ["--graph"]
    toFlags RelativeDate = ["--date=relative"]
    toFlags (MaxCount n) = ["--max-count="++show n]

revList :: [RevListOption] -> IO String
revList opts =
    do let flags = concatMap toFlags opts
       debugMessage "calling git-rev-list"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-rev-list" ("master":flags))
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git-rev-list failed"

revListHashes :: IO [Hash Commit]
revListHashes = do x <- revList []
                   return $ map (mkHash Commit) $ words x

-- | FIXME: I believe that clone is porcelain...

clone :: [String] -> IO ()
clone args =
    do debugMessage "calling git-clone"
       (Nothing, Nothing, Nothing, pid) <-
           createProcess (proc "git-clone" args)
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git-clone failed"

catBlob :: Hash Blob -> IO B.ByteString
catBlob (Hash Blob h) =
    do debugMessage "calling git-cat-file blob"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-cat-file" ["blob",h])
                             { std_out = CreatePipe }
       out <- B.hGetContents stdout
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return out
         ExitFailure _ -> fail "git-cat-file blob failed"

data TreeEntry = Subtree (Hash Tree)
               | File (Hash Blob)
               | Executable (Hash Blob)
instance Show TreeEntry where
    show (Subtree (Hash Tree h)) = "040000 tree "++h
    show (File (Hash Blob h)) = "100644 blob "++h
    show (Executable (Hash Blob h)) = "100755 blob "++h

catTree :: Hash Tree -> IO [(FileName, TreeEntry)]
catTree (Hash Tree h) =
    do debugMessage "calling git-cat-file tree"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-cat-file" ["-p",h])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> mapM parseit $ lines out
         ExitFailure _ -> fail "git-cat-file blob failed"
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
                _ -> fail "weird line in tree"

catCommitTree :: Hash Commit -> IO (Hash Tree)
catCommitTree (Hash Commit h) =
    do debugMessage "calling git-cat-file"
       (Nothing, Just stdout, Nothing, pid) <-
           createProcess (proc "git-cat-file" ["commit",h])
                             { std_out = CreatePipe }
       out <- hGetContents stdout
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> parseit out
         ExitFailure _ -> fail "git-cat-file blob failed"
    where parseit x =
              case splitAt 5 x of
                ("tree ",x') -> return $ mkHash Tree $ take 40 x'
                _ -> fail "weird stuff in commitTree"

hashObject :: (Handle -> IO ()) -> IO (Hash Blob)
hashObject wr =
    do debugMessage "calling git-hash-object"
       (Just i, Just o, Nothing, pid) <-
           createProcess (proc "git-hash-object" ["-w","--stdin"])
                             { std_in = CreatePipe,
                               std_out = CreatePipe }
       out <- hGetContents o
       wr i
       hClose i
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkHash Blob out
         ExitFailure _ -> fail ("git-hash-object failed\n"++out)

mkTree :: [(FileName, TreeEntry)] -> IO (Hash Tree)
mkTree xs =
    do debugMessage "calling git-mk-tree"
       (Just i, Just o, Nothing, pid) <-
           createProcess (proc "git-mktree" [])
                             { std_in = CreatePipe,
                               std_out = CreatePipe }
       out <- hGetContents o
       mapM_ (putStuff i) xs
       hClose i
       ec <- length out `seq` waitForProcess pid
       case ec of
         ExitSuccess -> return $ mkHash Tree out
         ExitFailure _ -> fail "git-mk-tree failed"
    where putStuff i (f, te) = hPutStr i (show te++'\t':fn2fp f++"\n")

gitApply :: FilePath -> IO ()
gitApply p =
    do debugMessage "calling git-apply"
       (Nothing, Nothing, Nothing, pid) <- createProcess (proc "git-apply" [p])
       ec <- waitForProcess pid
       case ec of
         ExitSuccess -> return ()
         ExitFailure _ -> fail "git-apply failed"
