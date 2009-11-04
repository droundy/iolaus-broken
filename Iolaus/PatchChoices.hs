-- Copyright (C) 2002-2004 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{-# LANGUAGE CPP #-}
-- , TypeOperators, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

#include "gadts.h"

-- | PatchChoices divides a sequence of patches into three sets: "first",
-- "middle" and "last", such that all patches can be applied, if you first
-- apply the first ones then the middle ones and then the last ones.
-- Obviously if there are dependencies between the patches that will put a
-- constraint on how you can choose to divide them up.  The PatchChoices data
-- type and associated functions are here to deal with many of the common
-- cases that come up when choosing a subset of a group of patches.
--
-- 'force_last' tells PatchChoices that a particular patch is required to be in
-- the "last" group, which also means that any patches that depend on it
-- must be in the "last" group.
--
-- Internally, a PatchChoices doesn't actually reorder the patches until it is
-- asked for the final output (e.g. by 'get_first_choice').  Instead, each
-- patch is placed in a state of definitely first, definitely last and
-- undecided; undecided leans towards "middle".  In case you're wondering
-- about the first-middle-last language, it's because in some cases the
-- "yes" answers will be last (as is the case for the revert command), and
-- in others first (as in record, pull and push).
module Iolaus.PatchChoices ( PatchChoices, patch_choices, patch_choices_tps,
                      patch_slot,
                      get_choices,
                      separate_first_middle_from_last,
                      separate_first_from_middle_last,
                      force_first, force_firsts, force_last, force_lasts,
                      force_matching_first, force_matching_last,
                      select_all_middles,
                      make_uncertain, make_everything_later,
                      TaggedPatch, Tag, tag, tp_patch,
                             Slot(..),
                    ) where

import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef ( newIORef, writeIORef, readIORef )
import Iolaus.Patch ( Patchy, commuteWhatWeCanRL, Apply, apply,
                      Commute, commute, merge, list_touched_files,
                      Invert, invert, identity )
import Iolaus.Ordered ( FL(..), RL(..), MyEq, unsafeCompare,
                             (:>)(..), (:\/:)(..), (:/\:)(..),
                             zipWithFL, mapFL_FL, mapFL,
                             (+>+), reverseRL, unsafeCoerceP )


newtype Tag = TG Integer deriving ( Num, Show, Eq, Ord, Enum )
data TaggedPatch p C(x y) = TP Tag (p C(x y))
data PatchChoice p C(x y) = PC (TaggedPatch p C(x y)) Slot
newtype PatchChoices p C(x y) = PCs (FL (PatchChoice p) C(x y))

data Slot = InFirst | InMiddle | InLast

invertTag :: Slot -> Slot
invertTag InFirst = InLast
invertTag InLast  = InFirst
invertTag t = t

tag :: TaggedPatch p C(x y) -> Tag
tag (TP (TG t) _) = TG t

tp_patch :: TaggedPatch p C(x y) -> p C(x y)
tp_patch (TP _ p) = p

liftTP :: (p C(x y) -> p C(a b)) -> (TaggedPatch p C(x y) -> TaggedPatch p C(a b))
liftTP f (TP t p) = TP t (f p)

instance MyEq p => MyEq (TaggedPatch p) where
    unsafeCompare (TP t1 p1) (TP t2 p2) = t1 == t2 && unsafeCompare p1 p2

instance Invert p => Invert (TaggedPatch p) where
    invert = liftTP invert
    identity = TP (-1) identity

instance Commute p => Commute (TaggedPatch p) where
    commute (TP t1 p1 :> TP t2 p2) = do p2' :> p1' <- commute (p1 :> p2)
                                        return (TP t2 p2' :> TP t1 p1')
    list_touched_files (TP _ p) = list_touched_files p
    merge (TP t1 p1 :\/: TP t2 p2) = do p2' :/\: p1' <- merge (p1 :\/: p2)
                                        Just $ TP t2 p2' :/\: TP t1 p1'

instance Apply p => Apply (TaggedPatch p) where
    apply (TP _ p) = apply p

patch_choices :: Patchy p => FL p C(x y) -> PatchChoices p C(x y)
patch_choices = fst . patch_choices_tps

patch_choices_tps
    :: Patchy p => FL p C(x y)
    -> (PatchChoices p C(x y), FL (TaggedPatch p) C(x y))
patch_choices_tps ps = let tps = zipWithFL TP [1..] ps
                       in (PCs $ zipWithFL (flip PC) (repeat InMiddle) tps, tps)

make_everything_later :: Patchy p => PatchChoices p C(x y) -> PatchChoices p C(x y)

instance MyEq p => MyEq (PatchChoice p) where
    unsafeCompare (PC tp1 _) (PC tp2 _) = unsafeCompare tp1 tp2

instance Invert p => Invert (PatchChoice p) where
    invert (PC tp mf) = PC (invert tp) (invertTag mf)
    identity = PC identity InMiddle

instance Commute p => Commute (PatchChoice p) where
    commute (PC t1 x1 :> PC t2 x2)
        = do t2' :> t1' <- commute (t1 :> t2)
             return (PC t2' x2 :> PC t1' x1)
    merge (PC t1 x1 :\/: PC t2 x2) = do t2' :/\: t1' <- merge (t1 :\/: t2)
                                        Just $ PC t2' x2 :/\: PC t1' x1
    list_touched_files (PC t _) = list_touched_files t

invertSeq :: (Invert p, Invert q) => (p :> q) C(x y) -> (q :> p) C(y x)
invertSeq (x :> y) = (invert y :> invert x)

separate_first_from_middle_last
    :: Patchy p => PatchChoices p C(x z)
    -> (FL (TaggedPatch p) :> FL (TaggedPatch p)) C(x z)
separate_first_from_middle_last (PCs e) = pull_only_firsts e

separate_first_middle_from_last :: Patchy p => PatchChoices p C(x z)
                                -> (FL (TaggedPatch p) :> FL (TaggedPatch p)) C(x z)
separate_first_middle_from_last (PCs e) = pull_firsts_middles e

get_choices :: Patchy p => PatchChoices p C(x y)
            -> (FL (TaggedPatch p) :> FL (TaggedPatch p) :> FL (TaggedPatch p)) C(x y)
get_choices (PCs e) = case pull_firsts e of
                      f :> ml -> case pull_firsts (invert ml) of
                                 l :> m -> f :> mapFL_FL pc2tp (invert m) :> invert l
  where pc2tp (PC tp _) = tp

pull_firsts_middles :: Patchy p => FL (PatchChoice p) C(x z) -> (FL (TaggedPatch p) :> FL (TaggedPatch p)) C(x z)
pull_firsts_middles easyPC =
    let r = unsafePerformIO
          $ newIORef (error "pull_firsts_middles called badly")
        f :: Patchy p => RL (TaggedPatch p) C(a x) -> FL (PatchChoice p) C(x z) -> FL (TaggedPatch p) C(a d)
        f acc NilFL = unsafePerformIO (writeIORef r (reverseRL acc)) `seq` (unsafeCoerceP NilFL)
        f acc (PC tp InLast:>:e) = f (tp:<:acc) e
        f acc (PC tp _:>:e) = case commuteWhatWeCanRL (acc :> tp) of
                              more :> tp' :> acc' -> reverseRL more+>+tp':>:f acc' e
        xs = f NilRL easyPC
    in (xs :> unsafePerformIO (readIORef r))

pull_only_firsts :: Patchy p => FL (PatchChoice p) C(x z) -> (FL (TaggedPatch p) :> FL (TaggedPatch p)) C(x z)
pull_only_firsts easyPC =
    let r = unsafePerformIO
          $ newIORef (error "pull_only_firsts called badly")
        f :: Patchy p => RL (TaggedPatch p) C(a x) -> FL (PatchChoice p) C(x z) -> FL (TaggedPatch p) C(a d)
        f acc NilFL = unsafePerformIO (writeIORef r (reverseRL acc)) `seq` (unsafeCoerceP NilFL)
        f acc (PC tp InFirst:>:e) = case commuteWhatWeCanRL (acc :> tp) of
                                        more :> tp' :> acc' -> reverseRL more+>+tp':>:f acc' e
        f acc (PC tp _:>:e) = f (tp:<:acc) e
        xs = f NilRL easyPC
    in (xs :> unsafePerformIO (readIORef r))

{-
pull_middles_lasts :: EasyPC p -> ([TaggedPatch p], [TaggedPatch p])
pull_middles_lasts easyPC =
    let r = unsafePerformIO
          $ newIORef (error "pull_middles_lasts called badly")
        f acc [] = unsafePerformIO (writeIORef r (reverse acc)) `seq` []
        f acc (PC tp (Just True):e) = f (tp:acc) e
        f acc (PC (TP t p) _:e) = case commute_up_list p acc of
                                  (acc', p') -> TP t p':f acc' e
        xs = f [] easyPC
    in (xs, unsafePerformIO (readIORef r))
-}

--pull_only_lasts :: EasyPC p -> ([TaggedPatch p], [TaggedPatch p])
--pull_only_lasts easyPC =
--    let r = unsafePerformIO
--          $ newIORef (error "pull_only_lasts called badly")
--        f acc [] = unsafePerformIO (writeIORef r (reverse acc)) `seq` []
--        f acc (PC (TP t p) (Just False):e) = case commute_up_list p acc of
--                                             (acc', p') -> TP t p':f acc' e
--        f acc (PC tp _:e) = f (tp:acc) e
--        xs = f [] easyPC
--    in (xs, unsafePerformIO (readIORef r))

pull_firsts :: Patchy p => FL (PatchChoice p) C(x z)
            -> (FL (TaggedPatch p) :>  FL (PatchChoice p)) C(x z)
pull_firsts e = case pull_first e of
                Nothing -> (NilFL :> e)
                Just (p:>e') -> case pull_firsts e' of
                                (ps:>e'') -> (p:>:ps :> e'')

pull_lasts :: Patchy p => FL (PatchChoice p) C(x y) -> (FL (PatchChoice p) :> FL (TaggedPatch p)) C(x y)
pull_lasts e = invertSeq $ pull_firsts $ invert e

pull_first :: Patchy p => FL (PatchChoice p) C(x z) -> Maybe ((TaggedPatch p :> FL (PatchChoice p)) C(x z))
pull_first NilFL = Nothing
pull_first (PC tp InFirst:>:e) = Just (tp :> e)
pull_first (PC (TP t p) InLast:>:e) =
    case pull_first e of
    Just (TP t2 p2 :> e') ->
        case commute (p:>p2) of
        Just (p2':>p') -> Just (TP t2 p2' :> PC (TP t p') InLast:>:e')
        Nothing -> error "Aaack fixme!"
    Nothing -> Nothing
pull_first (PC tp@(TP t p) InMiddle:>:e) =
    case pull_first e of
    Just (TP t2 p2 :> e') ->
        case commute (p:>p2) of
        Just (p2':>p') -> Just (TP t2 p2' :> (PC (TP t p') InMiddle:>:e'))
        Nothing -> Just (tp :> PC (TP (-t2) p2) InFirst:>:e')
    Nothing -> Nothing

patch_slot :: forall p C(a b x y). TaggedPatch p C(a b) -> PatchChoices p C(x y) -> Slot
patch_slot tp (PCs e) = ipf e
  where ipf :: FL (PatchChoice p) C(u v) -> Slot
        ipf (PC a mb:>:e') | tag a == tag tp = mb
                           | otherwise = ipf e'
        -- actually, the following should be impossible, but this is a reasonable answer
        ipf NilFL = InLast

set_simplys :: [Tag] -> Bool -> FL (PatchChoice p) C(x y) -> FL (PatchChoice p) C(x y)
set_simplys ts b e = mapFL_FL ch e
    where ch (PC tp@(TP t _) _)
           | t `elem` ts = PC tp (if b then InFirst else InLast)
           | otherwise   = PC tp InMiddle


m2ids :: (FORALL(x y) TaggedPatch p C(x y) -> Bool) -> FL (PatchChoice p) C(a b) -> [Tag]
m2ids m (PC tp@(TP t _) _:>:e)
 | m tp = t:m2ids m e
 | otherwise = m2ids m e
m2ids _ NilFL = []

force_matching_first :: Patchy p => (FORALL(x y) TaggedPatch p C(x y) -> Bool)
                     -> PatchChoices p C(a b) -> PatchChoices p C(a b)
force_matching_first m (PCs e) =
    let thd (PC (TP t _) _) = t
        xs = m2ids m e
        not_needed = case pull_firsts $ set_simplys xs True e of
                     _ :> rest -> mapFL thd rest
        ch pc@(PC tp@(TP t _) _)
         | t `elem` not_needed = pc
         | otherwise = PC tp InFirst
    in PCs $ mapFL_FL ch e

force_firsts :: Patchy p => [Tag] -> PatchChoices p C(x y) -> PatchChoices p C(x y)
force_firsts ps pc = force_matching_first ((`elem` ps) . tag) pc

force_first :: Patchy p => Tag -> PatchChoices p C(x y) -> PatchChoices p C(x y)
force_first p pc = force_matching_first ((== p) . tag) pc

select_all_middles :: Patchy p => Bool -> PatchChoices p C(x y) -> PatchChoices p C(x y)
select_all_middles b (PCs e) = PCs (mapFL_FL f e)
    where f (PC tp InMiddle) = PC tp (if b then InLast else InFirst)
          f pc = pc

reverse_pc :: Patchy p => PatchChoices p C(x y) -> PatchChoices p C(y x)
reverse_pc (PCs e) = PCs $ invert e

force_matching_last :: Patchy p => (FORALL(x y) TaggedPatch p C(x y) -> Bool)
                    -> PatchChoices p C(a b) -> PatchChoices p C(a b)
force_matching_last m (PCs e) =
    let thd (PC (TP t _) _) = t
        xs = m2ids m e
        not_needed = case pull_lasts $ set_simplys xs False e of
                     rest :> _ -> mapFL thd rest
        ch pc@(PC tp@(TP t _) _)
         | t `elem` not_needed = pc
         | otherwise = PC tp InLast
    in PCs $ mapFL_FL ch e

force_last :: Patchy p => Tag -> PatchChoices p C(x y) -> PatchChoices p C(x y)
force_last p pc = reverse_pc $ force_first p $ reverse_pc pc

force_lasts :: Patchy p => [Tag] -> PatchChoices p C(x y) -> PatchChoices p C(x y)
force_lasts ps pc = reverse_pc $ force_firsts ps $ reverse_pc pc

make_uncertain :: Patchy p => Tag -> PatchChoices p C(x y) -> PatchChoices p C(x y)
make_uncertain t (PCs e) = PCs $ mapFL_FL ch e
    where ch pc@(PC x _) = if t == tag x then PC x InMiddle else pc

make_everything_later (PCs e) = PCs $ mapFL_FL ch e
    where ch (PC tp InMiddle) = PC tp InLast
          ch (PC tp InFirst)  = PC tp InMiddle
          ch x = x
