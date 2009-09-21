#!/usr/bin/runhaskell
import Distribution.Franchise.V1

main = build [] $
       do hcFlags ["-Wall","-Iinclude"]
          withModule "System.Process.Redirects" $ define "HAVE_REDIRECTS"
          executable "arcs" "arcs.hs" []
