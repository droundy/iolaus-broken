{-# LANGUAGE CPP, TypeOperators #-}

#include "gadts.h"
module Iolaus.DeltaDebug ( largestPassingSet, smallestFailingSet,
                           smallestFailingChange ) where

import Data.List ( (\\), sort )
import System.IO.Unsafe ( unsafePerformIO )

import Iolaus.Flags ( Flag(Test) )
import Iolaus.Ordered ( (:>)(..), FL(..), (+>+), mapFL, mapFL_FL, reverseFL )
import Iolaus.Patch ( Patchy, Effect, showPatch, apply_to_slurpy,
                      commuteWhatWeCanToRightRLFL )
import Iolaus.PatchChoices ( TaggedPatch, Tag, tag, tp_patch, get_choices,
                             patch_choices, patch_choices_tps,
                             force_matching_first, force_matching_last,
                             separate_first_from_middle_last)
import Iolaus.Printer ( putDocLn, ($$), text )

import Git.Plumbing ( Hash, Tree )
import Git.Helpers ( writeSlurpTree, slurpTree, testPredicate, TestResult(..) )


largestPassingSet :: (Effect p, Patchy p) => Hash Tree C(x) -> FL p C(x y)
                  -> IO ((FL p :> FL p) C(x y))
largestPassingSet _ NilFL = return (NilFL :> NilFL)
largestPassingSet t0 xs0 =
    do t <- slurpTree t0 >>= apply_to_slurpy xs0 >>= writeSlurpTree
       p <- testPredicate [Test] t
       case p of
         Pass -> return (xs0 :> NilFL)
         _ -> lps t0 xs0

lps :: (Effect p, Patchy p) => Hash Tree C(x) -> FL p C(x y)
    -> IO ((FL p :> FL p) C(x y))
lps _ (x:>:NilFL) = return (NilFL :> x :>: NilFL)
lps _ NilFL = return (NilFL :> NilFL)
lps t0 xs0 =
    do sheep :> goats :> strays <- smallestFailingChange t0 xs0
       mok :> moregoats <- return $
           commuteWhatWeCanToRightRLFL (reverseFL goats :> strays)
       t <- slurpTree t0 >>= apply_to_slurpy sheep >>= writeSlurpTree
       moresheep :> othergoats <- lps t mok
       return (sheep +>+ moresheep :> othergoats +>+ moregoats)

smallestFailingSet :: Patchy p => Hash Tree C(x) -> FL p C(x y)
                   -> IO ((FL p :> FL p) C(x y))
smallestFailingSet _ xs = return (xs :> NilFL)

-- | this is what the delta-debugging algorithm provides, with no
-- guarantee as to whether the "first" or "last" sets are bigger or
-- smaller.

smallestFailingChange
    :: (Effect p, Patchy p) => Hash Tree C(x) -> FL p C(x y)
    -> IO ((FL p :> FL p :> FL p) C(x y))
smallestFailingChange t xs =
    do (txs, testit) <- ddpatches t xs
       let (sheept, goatst) = dd2 testit [] (mapFL tag txs) 2
       case get_choices $
            force_matching_last ((`notElem` (sheept++goatst)) . tag) $
            force_matching_first ((`elem` sheept) . tag) $ patch_choices xs of
         sheep :> goats :> strays ->
             do putDocLn $ text "found sheep:" $$
                         showPatch (mapFL_FL tp_patch sheep)
                putDocLn $ text "found goats:" $$
                         showPatch (mapFL_FL tp_patch goats)
                putDocLn $ text "strays:" $$
                         showPatch (mapFL_FL tp_patch strays)
                return (mapFL_FL tp_patch sheep :>
                        mapFL_FL tp_patch goats :>
                        mapFL_FL tp_patch strays)

ddpatches :: Patchy p => Hash Tree C(x) -> FL p C(x y)
          -> IO (FL (TaggedPatch p) C(x y), [Tag] -> TestResult)
ddpatches t ps = return (tps, unsafePerformIO . testtags)
    where (pc, tps) = patch_choices_tps ps
          testtags :: [Tag] -> IO TestResult
          testtags ts =
              case separate_first_from_middle_last $
                   force_matching_first ((`elem` ts) . tag) pc of
                xs :> _ | sort (mapFL tag xs) == sort ts ->
                            do t' <- slurpTree t >>= apply_to_slurpy xs
                                     >>= writeSlurpTree
                               testPredicate [Test] t'
                _ -> return Unresolved

dd2 :: (Show a, Eq a) => ([a] -> TestResult) -> [a] -> [a] -> Int -> ([a],[a])
dd2 _ ok [b] _ = (ok, [b])
dd2 f ok bad n =
 case map snd (filter ((== Pass) . fst) $ scoreLess) of
 mygood:_ -> dd2 f (mygood++ok) (bad\\mygood) 2
 [] -> case map snd $ filter ((== Pass) . fst) scoreBads of
       mygood:_ -> dd2 f (mygood++ok) (bad\\mygood) (max (n-1) 2)
       [] -> case map snd $ filter ((== Fail) . fst) scoreBads of
             mybad:_ -> dd2 f ok mybad 2
             [] -> case map snd $ filter ((== Fail) . fst) $ scoreLess of
                   mybad:_ -> dd2 f ok mybad (max (n-1) 2)
                   [] -> if n < length bad
                         then dd2 f ok bad (min (2*n) (length bad))
                         else (ok,bad)
 where splitit [] = []
       splitit xs = take nn xs : splitit (drop nn xs)
       bads = splitit bad
       score bs = map (\b -> f (ok++b)) bs `zip` bs
       lessbads = map (\x -> take x bad ++ drop (x+nn) bad) [0,nn ..length bad]
       nn = max 1 $ length bad `div` n
       scoreLess = score lessbads
       scoreBads = score bads
