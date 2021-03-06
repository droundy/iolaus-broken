-- Copyright (C) 2002-2003,2009 David Roundy
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

{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- , ScopedTypeVariables, TypeOperators, PatternGuards #-}

#include "gadts.h"

module Iolaus.SelectChanges ( with_selected_changes,
                            with_selected_last_changes_to_files,
                            with_selected_last_changes_reversed,
                            with_selected_changes_to_files ) where
import System.IO
import Data.List ( intersperse )
import Data.Maybe ( catMaybes )
import Data.Char ( toUpper )
import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import Iolaus.English ( Noun(..), englishNum  )
import Iolaus.Patch ( Patchy, Prim, Effect, summarize, invert,
                      showContextPatch, list_touched_files, apply_to_slurpy )
import qualified Iolaus.Patch ( thing, things )
import Iolaus.Ordered ( FL(..), RL(..), (:>)(..),
                       (+>+), lengthFL, mapFL_FL,
                       spanFL, reverseFL, (+<+), mapFL )
import Iolaus.PatchChoices ( PatchChoices, patch_choices, patch_choices_tps,
                           force_first, force_last, make_uncertain, tag,
                           get_choices, separate_first_middle_from_last,
                           separate_first_from_middle_last,
                           patch_slot, select_all_middles,
                           TaggedPatch, tp_patch, Slot(..) )
import Iolaus.TouchesFiles ( deselect_not_touching, select_not_touching )
import Iolaus.SlurpDirectory ( Slurpy )
import Iolaus.Flags ( Flag( Summary, Verbose ), isInteractive )
import Iolaus.Utils ( askUser, promptCharFancy, without_buffering )
import Iolaus.Printer ( prefix, putDocLn )
#include "impossible.h"

data WhichChanges = Last | LastReversed | First | FirstReversed deriving (Eq, Show)

type MatchCriterion p = FORALL(u v) WhichChanges -> [Flag] -> (p C(u v)) -> Bool

type WithPatches p a C(x y) =
        String              -- jobname
     -> [Flag]         -- opts
     -> Slurpy C(x)          -- directory
     -> FL p C(x y)         -- patches to select among
     -> ((FL p :> FL p) C(x y) -> IO a) -- job
     -> IO a                -- result of running job

-- | The only difference with 'WithPatches' is the [FilePath] argument
type WithPatchesToFiles p a C(x y) =
        String              -- jobname
     -> [Flag]         -- opts
     -> Slurpy C(x)         -- directory
     -> [FilePath]          -- files
     -> FL p C(x y)         -- patches to select among
     -> ((FL p :> FL p) C(x y) -> IO a) -- job
     -> IO a                -- result of running job

with_selected_changes :: WithPatches Prim a C(x y)
with_selected_changes_to_files :: WithPatchesToFiles Prim a C(x y)
with_selected_last_changes_to_files :: WithPatchesToFiles Prim a C(x y)
with_selected_last_changes_reversed :: WithPatches Prim a C(x y)

-- Common match criteria
triv :: MatchCriterion p
triv _ _ _ = True

with_selected_changes = wasc First triv
with_selected_changes_to_files = wasc_ First triv
with_selected_last_changes_to_files = wasc_ Last triv
with_selected_last_changes_reversed = wasc  LastReversed triv

-- | wasc and wasc_ are just shorthand for with_any_selected_changes
wasc  :: (Patchy p, Effect p) =>
         WhichChanges -> MatchCriterion p -> WithPatches p a C(x y)
wasc mwch crit j o s = wasc_ mwch crit j o s []
wasc_ :: (Patchy p, Effect p) =>
         WhichChanges -> MatchCriterion p -> WithPatchesToFiles p a C(x y)
wasc_ = with_any_selected_changes

with_any_selected_changes
    :: (Patchy p, Effect p) =>
       WhichChanges -> MatchCriterion p -> WithPatchesToFiles p a C(x y)
with_any_selected_changes Last crit jn opts s fs =
    with_any_selected_changes_last
        (patches_to_consider_last' fs)
        crit jn opts s fs
with_any_selected_changes First crit jn opts s fs =
    with_any_selected_changes_first
       (patches_to_consider_first' fs)
       crit jn opts s fs
with_any_selected_changes FirstReversed crit jn opts s fs =
    with_any_selected_changes_first_reversed
       (patches_to_consider_first_reversed' fs)
       crit jn opts s fs
with_any_selected_changes LastReversed crit jn opts s fs =
    with_any_selected_changes_last_reversed
        (patches_to_consider_last_reversed' fs)
        crit jn opts s fs


data KeyPress a = KeyPress { kp     :: Char
                           , kpHelp :: String }

helpFor :: String -> [[KeyPress a]] -> String
helpFor jobname options =
  unlines $ [ "How to use "++jobname++":" ]
            ++ (concat $ intersperse [""] $ map (map help) options)
            ++ [ ""
               , "?: show this help"
               , ""
               , "<Space>: accept the current default (which is capitalized)"
               ]
  where help i = kp i:(": "++kpHelp i)

keysFor :: [[KeyPress a]] -> [Char]
keysFor = concatMap (map kp)

with_any_selected_changes_last :: (Patchy p, Effect p)
                               => (FL p C(x y) -> (FL p :> FL p) C(x y))
                               -> MatchCriterion p
                               -> WithPatchesToFiles p a C(x y)
with_any_selected_changes_last p2c crit jobname opts s _ ps job =
 case p2c ps of
 ps_to_consider :> other_ps ->
     if not $ isInteractive opts
     then job $ ps_to_consider :> other_ps
     else do pc <- without_buffering $
                   tentatively_text_select "" jobname (Noun "patch") Last crit
                               opts ps_len 0
                               (fromJust $ apply_to_slurpy ps_to_consider s)
                               NilRL init_tps init_pc
             job $ selected_patches_last rejected_ps pc
     where rejected_ps = ps_to_consider
           ps_len = lengthFL init_tps
           (init_pc, init_tps) = patch_choices_tps $ other_ps

with_any_selected_changes_first :: forall p a C(x y). (Patchy p, Effect p)
                                => (FL p C(x y) -> (FL p :> FL p) C(x y))
                                -> MatchCriterion p
                                -> WithPatchesToFiles p a C(x y)
with_any_selected_changes_first p2c crit jobname opts s _ ps job =
 case p2c ps of
 ps_to_consider :> other_ps ->
     if not $ isInteractive opts
     then job $ ps_to_consider :> other_ps
     else do pc <- without_buffering $
                   tentatively_text_select "" jobname (Noun "patch") First crit
                                          opts ps_len 0 s NilRL init_tps init_pc
             job $ selected_patches_first rejected_ps pc
     where rejected_ps = other_ps
           ps_len = lengthFL init_tps
           (init_pc, init_tps) = patch_choices_tps $ ps_to_consider

with_any_selected_changes_first_reversed :: forall p a C(xxx yxx).
                                            (Patchy p, Effect p)
                                => (FL p C(xxx yxx) -> (FL p :> FL p) C(yxx xxx))
                                -> MatchCriterion p
                                -> WithPatchesToFiles p a C(xxx yxx)
with_any_selected_changes_first_reversed p2c crit jobname opts s _ ps job =
 case p2c ps of
 ps_to_consider :> other_ps ->
     if not $ isInteractive opts
     then job $ invert other_ps :> invert ps_to_consider
     else do pc <- without_buffering $
                   tentatively_text_select "" jobname (Noun "patch")
                              FirstReversed crit opts ps_len 0
                              (fromJust $ apply_to_slurpy (invert other_ps) s)
                              NilRL init_tps init_pc
             job $ selected_patches_first_reversed rejected_ps pc
     where rejected_ps = ps_to_consider
           ps_len = lengthFL init_tps
           (init_pc, init_tps) = patch_choices_tps other_ps

with_any_selected_changes_last_reversed
    :: forall p a C(xfirst yfirst). (Patchy p, Effect p)
       => (FL p C(xfirst yfirst) -> (FL p :> FL p) C(yfirst xfirst))
    -> MatchCriterion p -> WithPatchesToFiles p a C(xfirst yfirst)
with_any_selected_changes_last_reversed p2c crit jobname opts s _ ps job =
 case p2c ps of
   ps_to_consider :> other_ps ->
     if not $ isInteractive opts
     then job $ invert other_ps :> invert ps_to_consider
     else do pc <- without_buffering $
                   tentatively_text_select "" jobname (Noun "patch") LastReversed crit
                                         opts ps_len 0
                                         (fromJust $ apply_to_slurpy ps s)
                                         NilRL init_tps init_pc
             job $ selected_patches_last_reversed rejected_ps pc
     where rejected_ps = other_ps
           ps_len = lengthFL init_tps
           (init_pc, init_tps) = patch_choices_tps ps_to_consider


patches_to_consider_first' :: (Patchy p, Effect p)
                     => [FilePath]  -- ^ files
                     -> FL p C(x y) -- ^ patches
                     -> (FL p :> FL p) C(x y)
patches_to_consider_first' fs ps =
  if null fs
  then ps :> NilFL
  else tp_patches $ separate_first_middle_from_last $ deselect_not_touching fs
           $ patch_choices ps

patches_to_consider_last' :: (Patchy p, Effect p)
                     => [FilePath]  -- ^ files
                     -> FL p C(x y) -- ^ patches
                     -> (FL p :> FL p) C(x y)
patches_to_consider_last' fs ps =
  if null fs
  then NilFL :> ps
  else case get_choices $ select_not_touching fs $ patch_choices ps of
         fc :> mc :> lc -> tp_patches $ fc :> mc +>+ lc

patches_to_consider_first_reversed' :: (Patchy p, Effect p)
                     => [FilePath]  -- ^ files
                     -> FL p C(x y) -- ^ patches
                     -> (FL p :> FL p) C(y x)
patches_to_consider_first_reversed' fs ps =
  if null fs
  then NilFL :> (invert ps)
  else case get_choices $ select_not_touching fs $ patch_choices $ invert ps of
         fc :> mc :> lc -> tp_patches $ fc :> mc +>+ lc

patches_to_consider_last_reversed' :: (Patchy p, Effect p)
                     => [FilePath]  -- ^ files
                     -> FL  p C(x y) -- ^ patches
                     -> (FL p :> FL p) C(y x)
patches_to_consider_last_reversed' fs ps =
  if null fs
  then (invert ps) :> NilFL
  else tp_patches $ separate_first_middle_from_last $ deselect_not_touching fs
           $ patch_choices $ invert ps

-- | Returns the results of a patch selection user interaction
selected_patches_last :: (Patchy p, Effect p) =>
                         FL p C(x y) -> PatchChoices p C(y z)
                      -> (FL p :> FL p) C(x z)
selected_patches_last other_ps pc =
  case get_choices pc of
   fc :> mc :> lc -> other_ps +>+ mapFL_FL tp_patch (fc +>+ mc) :> mapFL_FL tp_patch lc

selected_patches_first :: (Patchy p, Effect p) =>
                          FL p C(y z) -> PatchChoices p C(x y)
                       -> (FL p :> FL p) C(x z)
selected_patches_first other_ps pc =
  case separate_first_from_middle_last pc of
  xs :> ys -> mapFL_FL tp_patch xs :> mapFL_FL tp_patch ys +>+ other_ps

selected_patches_last_reversed :: (Patchy p, Effect p) =>
                                  FL p C(y x) -> PatchChoices p C(z y)
                               -> (FL p :> FL p) C(x z)
selected_patches_last_reversed other_ps pc =
  case separate_first_from_middle_last pc of
  xs :> ys -> invert (mapFL_FL tp_patch ys +>+ other_ps) :> invert (mapFL_FL tp_patch xs)

selected_patches_first_reversed :: (Patchy p, Effect p) =>
                                   FL p C(z y) -> PatchChoices p C(y x)
                                -> (FL p :> FL p) C(x z)
selected_patches_first_reversed other_ps pc =
  case get_choices pc of
  fc :> mc :> lc -> invert (mapFL_FL tp_patch lc) :> invert (other_ps +>+ mapFL_FL tp_patch (fc +>+ mc))

-- | 'printPatch' prints a patch, together with its context,
-- on standard output.
printPatch :: Patchy p => Slurpy C(x) -> p C(x y) -> IO ()
printPatch s p = putDocLn $ showContextPatch s p

text_select :: forall p C(x y z). (Patchy p, Effect p) => String -> WhichChanges
            ->  MatchCriterion p -> [Flag] -> Int -> Int -> Slurpy C(x)
            -> RL (TaggedPatch p) C(x y) -> FL (TaggedPatch p) C(y z) -> PatchChoices p C(x z)
            -> IO ((PatchChoices p) C(x z))
text_select _ _ _ _ _ _ _ _ NilFL pc = return pc
text_select jn whichch crit opts n_max n
            s tps_done tps_todo@(tp:>:tps_todo') pc = do
      viewp printPatch
      repeat_this -- prompt the user
    where
        do_next_action ja je = tentatively_text_select ja jn je whichch crit opts
                                          n_max
                                          (n+1) s (tp:<:tps_done) tps_todo'
        do_next = do_next_action "" (Noun "patch")
        helper :: p C(x y)
        helper = undefined
        thing  = Iolaus.Patch.thing helper
        things = Iolaus.Patch.things helper
        options_basic =
           [ KeyPress 'y' (jn++" this "++thing)
           , KeyPress 'n' ("don't "++jn++" it")
           , KeyPress 'w' ("wait and decide later, defaulting to no") ]
        options_file =
           [ KeyPress 's' ("don't "++jn++" the rest of the changes to this file")
           , KeyPress 'f' (jn++" the rest of the changes to this file") ]
        options_view =
           [ KeyPress 'v' ("view this "++thing++" in full")
           , KeyPress 'l' ("list all selected "++things) ]
        options_summary =
           [ KeyPress 'x' ("view a summary of this "++thing) ]
        options_quit =
           [ KeyPress 'd' (jn++" selected "++things++", skipping all the remaining "++things)
           , KeyPress 'a' (jn++" all the remaining "++things)
           , KeyPress 'q' ("cancel "++jn) ]
        options_nav =
           [ KeyPress 'j' ("skip to next "++thing)
           , KeyPress 'k' ("back up to previous "++thing) ]
        options = [options_basic]
                  ++ (if is_single_file_patch then [options_file] else [])
                  ++ [options_view ++
                      if Summary `elem` opts then [] else options_summary]
                  ++ [options_quit]
                  ++ [options_nav ]
        prompt = "Shall I "++jn++" this "++thing++"? "
               ++ "(" ++ show (n+1) ++ "/" ++ show n_max ++ ") "
        repeat_this :: IO ((PatchChoices p) C(x z))
        repeat_this = do
          yorn <- promptCharFancy prompt (keysFor options) (Just the_default) "?h"
          case yorn of
            'y' -> do_next $ force_yes (tag tp) pc
            'n' -> do_next $ force_no (tag tp) pc
            'w' -> do_next $ make_uncertain (tag tp) pc
            's' -> do_next_action "Skipped"  (Noun "change") $ skip_file
            'f' -> do_next_action "Included" (Noun "change") $ do_file
            'v' -> viewp printPatch >> repeat_this
            'l' ->
                do putStrLn $ "---- Already selected "++things++" ----"
                   case get_choices pc of
                     (first_chs:>mid:>last_chs) ->
                         if whichch == Last ||
                            whichch == FirstReversed
                         then do s' <- apply_to_slurpy (first_chs+>+mid) s
                                 printPatch s' $ mapFL_FL tp_patch last_chs
                         else printPatch s $ mapFL_FL tp_patch first_chs
                   putStrLn $ "---- end of already selected "++things++" ----"
                   viewp printPatch
                   repeat_this
            'x' -> do viewp (const $ putDocLn . prefix "    " . summarize)
                      repeat_this
            'd' -> return pc
            'a' -> do ask_confirmation
                      return $ select_all_middles (whichch == Last || whichch == FirstReversed) pc
            'q' -> do putStrLn $ jn_cap++" cancelled."
                      exitWith $ ExitSuccess
            'j' -> case tps_todo' of
                       NilFL -> -- May as well work out the length now we have all
                                -- the patches in memory
                                text_select jn whichch crit opts
                                    n_max n s tps_done tps_todo pc
                       _ -> text_select jn whichch crit opts
                                n_max (n+1) s (tp:<:tps_done) tps_todo' pc
            'k' -> case tps_done of
                        NilRL -> repeat_this
                        (tp':<:tps_done') ->
                           text_select jn whichch crit opts
                               n_max (n-1) s tps_done' (tp':>:tps_todo) pc
            'c' -> text_select jn whichch crit opts
                                        n_max n s tps_done tps_todo pc
            _   -> do putStrLn $ helpFor jn options
                      repeat_this
        force_yes = if whichch == Last || whichch == FirstReversed then force_last else force_first
        force_no  = if whichch == Last || whichch == FirstReversed then force_first else force_last
        patches_to_skip = (tag tp:) $ catMaybes
                        $ mapFL (\tp' -> if list_touched_files tp' == touched_files
                                         then Just (tag tp')
                                         else Nothing) tps_todo'
        skip_file = foldr force_no pc patches_to_skip
        do_file = foldr force_yes pc patches_to_skip
        the_default = get_default (whichch == Last || whichch == FirstReversed) $ patch_slot tp pc
        jn_cap = (toUpper $ head jn) : tail jn
        touched_files = list_touched_files $ tp_patch tp
        is_single_file_patch = length touched_files == 1
        viewp :: (FORALL(xx yy) Slurpy C(xx) -> p C(xx yy) -> IO ()) -> IO ()
        viewp f = if whichch == LastReversed || whichch == FirstReversed
                    then f (fromJust $ apply_to_slurpy (tp :<: tps_done) s)
                           (invert (tp_patch tp))
                    else f (fromJust $ apply_to_slurpy tps_done s)
                           (tp_patch tp)
        ask_confirmation =
            if jn `elem` ["unpull", "unrecord", "obliterate"]
            then do yorn <- askUser $ "Really " ++ jn ++ " all undecided patches? "
                    case yorn of
                     ('y':_) -> return ()
                     _ -> exitWith $ ExitSuccess
            else return ()

tentatively_text_select :: (Patchy p, Effect p) =>
                           String -> String -> Noun -> WhichChanges
                        -> MatchCriterion p -> [Flag]
                        -> Int -> Int -> Slurpy C(x)
                        -> RL (TaggedPatch p) C(x y)
                        -> FL (TaggedPatch p) C(y z)
                        -> PatchChoices p C(x z)
                        -> IO ((PatchChoices p) C(x z))
tentatively_text_select _ _ _ _ _ _ _ _ _ _ NilFL pc = return pc
tentatively_text_select jobaction jobname jobelement whichch crit
                        opts n_max n s ps_done ps_todo pc =
  case spanFL (\p -> decided $ patch_slot p pc) ps_todo of
  skipped :> unskipped -> do
   when (numSkipped > 0) show_skipped
   text_select jobname whichch crit opts n_max (n + numSkipped)
                   s (reverseFL skipped +<+ ps_done) unskipped pc
   where
   numSkipped  = lengthFL skipped
   show_skipped = do putStrLn $ _doing_ ++ _with_ ++ "."
                     when (Verbose `elem` opts) $ showskippedpatch skipped
    where
      _doing_  = _action_ ++ " " ++ jobname
      _with_   = " of " ++ show numSkipped ++ " " ++ _elem_ ""
      _action_ = if (length jobaction) == 0 then "Skipped" else jobaction
      _elem_ = englishNum numSkipped jobelement
      showskippedpatch :: (Patchy p, Effect p) =>
                          FL (TaggedPatch p) C(y t) -> IO ()
      showskippedpatch (tp:>:tps) = (putDocLn $ prefix "    " $ summarize (tp_patch tp)) >> showskippedpatch tps
      showskippedpatch NilFL = return ()

decided :: Slot -> Bool
decided InMiddle = False
decided _ = True

get_default :: Bool -> Slot -> Char
get_default _ InMiddle = 'w'
get_default True InFirst  = 'n'
get_default True InLast   = 'y'
get_default False InFirst = 'y'
get_default False InLast  = 'n'

tp_patches :: (FL (TaggedPatch p) :> FL (TaggedPatch p)) C(x y)
           -> (FL p :> FL p) C(x y)
tp_patches (x:>y) = mapFL_FL tp_patch x :> mapFL_FL tp_patch y
