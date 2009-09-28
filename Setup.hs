#!/usr/bin/runhaskell
import Distribution.Franchise.V1

main = build [] $
       do hcFlags ["-Wall","-Iinclude"]
          ghcFlags ["-threaded"]
          withDirectory "etc" $ etc "bash_completion.d/grit"
          withModule "System.Process.Redirects" $ define "HAVE_REDIRECTS"
          executable "grit" "grit.hs" []
          enforceAllPrivacy
