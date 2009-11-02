{-# LANGUAGE CPP, TypeOperators #-}

#include "gadts.h"
module Iolaus.DeltaDebug ( largestPassingSet, smallestFailingSet,
                           smallestFailingChange ) where

import Iolaus.Flags ( Flag(Test) )
import Iolaus.Ordered ( (:>)(..), FL(..), (+>+) )
import Iolaus.Patch ( Patchy, apply_to_slurpy, commuteWhatWeCanFL )

import Git.Plumbing ( Hash, Tree )
import Git.Helpers ( writeSlurpTree, slurpTree, testPredicate )

-- | Eventually, I plan to implement true delta debugging here, which
-- gives better scaling, but for now I'll just use a partial
-- exhaustive search.

largestPassingSet :: Patchy p => Hash Tree C(x) -> FL p C(x y)
                  -> IO ((FL p :> FL p) C(x y))
largestPassingSet t0 xs0 =
    do tall <- slurpTree t0 >>= apply_to_slurpy xs0 >>= writeSlurpTree
       passesAll <- testPredicate [Test] tall
       case passesAll of
         Just _ -> return (xs0 :> NilFL)
         Nothing -> lps t0 xs0
    where -- lps "knows" that the "all" case fails.
          lps :: Patchy p => Hash Tree C(x) -> FL p C(x y)
              -> IO ((FL p :> FL p) C(x y))
          lps t (x:>:xs) =
              do xt <- slurpTree t >>= apply_to_slurpy x >>= writeSlurpTree
                 passesXt <- testPredicate [Test] xt
                 case passesXt of
                   Just _ -> do ps :> fs <- lps xt xs
                                return (x:>:ps :> fs)
                   Nothing ->
                       case commuteWhatWeCanFL (x :> xs) of
                         mp :> x' :> f ->
                             do p :> f2 <- lps t mp
                                return (p :> f2 +>+ x' :>: f)
          lps _ NilFL = return (NilFL :> NilFL)

smallestFailingSet :: Patchy p => Hash Tree C(x) -> FL p C(x y)
                   -> IO ((FL p :> FL p) C(x y))
smallestFailingSet _ xs = return (xs :> NilFL)

-- | this is what the delta-debugging algorithm provides, with no
-- guarantee as to whether the "first" or "last" sets are bigger or
-- smaller.

smallestFailingChange
    :: Patchy p => Hash Tree C(x) -> FL p C(x y)
    -> IO ((FL p :> FL p :> FL p) C(x y))
smallestFailingChange t xs =
    do xsp :> xsf1 <- largestPassingSet t xs
       st <- slurpTree t
       st' <- apply_to_slurpy xsp st
       t' <- writeSlurpTree st'
       xsf :> xsmore <- smallestFailingSet t' xsf1
       return (xsp :> xsf :> xsmore)
