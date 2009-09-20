#!/usr/bin/runhaskell
import Distribution.Franchise.V1

main = build [] $ executable "arcs" "arcs.hs" []
