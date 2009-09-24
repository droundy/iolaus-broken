module Git.Helpers ( testIndex, slurpTree ) where

import System.Directory ( getCurrentDirectory, setCurrentDirectory )
import System.Process.Redirects ( system )
import System.Exit ( ExitCode(..) )
import System.IO.Unsafe ( unsafeInterleaveIO )

import Git.Plumbing ( Hash, Tree, TreeEntry(..),
                      checkoutCopy, catTree, catBlob )

import Arcs.FileName ( FileName )
import Arcs.SlurpDirectoryInternal
    ( Slurpy(..), SlurpyContents(..), ExecutableBit(..), slurpies_to_map )

testIndex :: IO ()
testIndex = do checkoutCopy "/tmp/testing/"
               here <- getCurrentDirectory
               setCurrentDirectory "/tmp/testing"
               ec <- system "./.git-hooks/test"
               case ec of
                 ExitFailure _ -> fail "test failed"
                 ExitSuccess -> return ()
               setCurrentDirectory here
               system "rm -rf /tmp/testing"
               return ()

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
