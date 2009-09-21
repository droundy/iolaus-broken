#!/usr/bin/runhaskell
import Distribution.Franchise.V1

main = build [] $ do hcFlags ["-Wall"]
                     executable "arcs" "arcs.hs" []
