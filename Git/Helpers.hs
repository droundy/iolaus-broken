module Git.Helpers ( testIndex ) where

import System.Directory ( getCurrentDirectory, setCurrentDirectory )
import System.Process.Redirects ( system )
import System.Exit ( ExitCode(..) )

import Git.Plumbing ( checkoutCopy )

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
