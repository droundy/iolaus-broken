-- Copyright (C) 2002-2004,2009 David Roundy
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

{-# OPTIONS_GHC -cpp -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Iolaus.Patch.Viewing ( summarize ) where

import Prelude hiding ( pi )
import Data.List ( sort )

import Iolaus.SlurpDirectory ( Slurpy, get_slurp, get_filecontents )
import Iolaus.ByteStringUtils ( linesPS, unlinesPS )
import qualified Data.ByteString as B ( ByteString, null, concat )
import Iolaus.FileName ( FileName, fp2fn, fn2fp )
import Iolaus.Printer
    ( Doc, empty, vcat, text, blueText, Color(Red,Green), colorPS,
      minus, plus, ($$), (<+>), (<>),
      prefix, renderString, unsafePackedString )
import Iolaus.Patch.Core ( Named(..) )
import Iolaus.Patch.Prim ( Prim(..), formatFileName, showPrim,
                           Effect, effect,
                           DirPatchType(..), FilePatchType(..) )
import Iolaus.Patch.Patchy ( Patchy, Apply, ShowPatch(..) )
import Iolaus.Patch.Apply ( apply_to_slurpy, chunkify )
#include "impossible.h"
#include "gadts.h"
import Iolaus.Ordered ( RL(..), FL(..), mapFL, reverseRL )

instance ShowPatch Prim where
    showPatch = showPrim
    showContextPatch s p@(FP _ (Chunk _ _ _ _)) = 
        showContextStuff s (p :>: NilFL)
    showContextPatch _ p = showPatch p
    summary = gen_summary . (:>: NilFL)
    thing _ = "change"

summarize :: Effect e => e C(x y) -> Doc
summarize = gen_summary . effect

showContextStuff :: Slurpy C(x) -> FL Prim C(x y) -> Doc
showContextStuff _ NilFL = empty
showContextStuff s0 ps@(FP f (Chunk c _ _ _) :>: _) =
    case (chunkify c . get_filecontents) `fmap` get_slurp f s0 of
      Nothing -> error "bad slurp showContextStuff"
      Just zs -> blueText "chunk" <+> formatFileName f $$
                 scc 0 zs s0 ps
    where scc :: Int -> [B.ByteString] -> Slurpy C(x) -> FL Prim C(x y)
              -> Doc
          scc w0 ws s (FP f' (Chunk c' w o n) :>: xs)
              | f' == f && c' == c =
                  precontext <> 
                  (colorPS Red $ B.concat o) <>
                  (colorPS Green $ B.concat n) <>
                  scc (w+length n) (drop (w+length o-w0) ws)
                      (fromJust $ apply_to_slurpy (FP f' (Chunk c' w o n)) s) xs
              where precontext =
                        if length prels > 6
                        then if w0 == 0
                             then formatFileName f <+> blueText (show w++":") $$
                                  unsafePackedString
                                  (unlinesPS $ drop (length prels-3) prels)
                             else (unsafePackedString $ unlinesPS $ take 3 $
                                   linesPS $ B.concat ws) $$
                                   formatFileName f<+>blueText (show w++":") $$
                                   unsafePackedString
                                   (unlinesPS $ drop (length prels-3) prels)
                        else unsafePackedString (unlinesPS prels)
                    prels = linesPS $ B.concat $ take (w-w0) ws
          scc _ ws s xs = case reverse first3 of
                          x:_ | B.null x ->
                                  (unsafePackedString $ unlinesPS first3) <>
                                  showContextStuff s xs
                          _ -> (unsafePackedString $ unlinesPS first3) $$
                               showContextStuff s xs
                  where first3 = take 3 $ linesPS $ B.concat ws

showContextStuff s (p :>: ps) = showContextPatch s p $$
                                showContextStuff snew ps
    where snew = fromJust $ apply_to_slurpy p s

gen_summary :: FL Prim C(x y) -> Doc
gen_summary p
    = vcat themoves
   $$ vcat themods
    where themods = map summ $ combine $ sort $ concat $ mapFL s p
          s :: Prim C(x y) -> [(FileName, Int, Int, Int, Bool)]
          s (FP f (Chunk _ _ o n)) = [(f, length o, length n, 0, False)]
          s (FP f (Chmod _)) = [(f, 0, 0, 0, False)]
          s (FP f AddFile) = [(f, -1, 0, 0, False)]
          s (FP f RmFile) = [(f, 0, -1, 0, False)]
          s (DP d AddDir) = [(d, -1, 0, 0, True)]
          s (DP d RmDir) = [(d, 0, -1, 0, True)]
          s (Move _ _) = [(fp2fn "", 0, 0, 0, False)]
          s Identity = [(fp2fn "", 0, 0, 0, False)]
          (-1) .+ _ = -1
          _ .+ (-1) = -1
          a .+ b = a + b
          combine ((f,a,b,r,isd):(f',a',b',r',_):ss)
              | f == f' = combine ((f,a.+a',b.+b',r+r',isd):ss)
          combine ((f,a,b,r,isd):ss) = (f,a,b,r,isd) : combine ss
          combine [] = []

          summ (f,_,-1,_,False) = text "R" <+> text (fn2fp f)
          summ (f,-1,_,_,False) = text "A" <+> text (fn2fp f)
          summ (f,0,0,0,False) | f == fp2fn "" = empty
          summ (f,a,b,r,False) = text "M" <+> text (fn2fp f)
                                 <+> rm a <+> ad b <+> rp r
          summ (f,_,-1,_,True) = text "R" <+> text (fn2fp f) <> text "/"
          summ (f,-1,_,_,True) = text "A" <+> text (fn2fp f) <> text "/"
          summ _ = empty
          ad 0 = empty
          ad a = plus <> text (show a)
          rm 0 = empty
          rm a = minus <> text (show a)
          rp 0 = empty
          rp a = text "r" <> text (show a)
          drop_dotslash ('.':'/':str) = drop_dotslash str
          drop_dotslash str = str
          themoves :: [Doc]
          themoves = mapFL showmoves p
          showmoves :: Prim C(x y) -> Doc
          showmoves (Move a b) = text " "    <> text (fn2fp a)
                                 <> text " -> " <> text (fn2fp b)
          showmoves _ = empty

instance (Effect p, ShowPatch p, Show n) => ShowPatch (Named n p) where
    showPatch (NamedP n p) = text (show n) $$ showPatch p
    showContextPatch s (NamedP n p) = text (show n) $$ showContextPatch s p
    description (NamedP n _) = text (show n)
    summary p = description p $$ text "" $$
                prefix "    " (summarize p)
    -- this isn't summary because summary does the
    -- wrong thing with (Named (FL p)) so that it can
    -- get the summary of a sequence of named patches
    -- right.
    showNicely p@(NamedP _ pt) = description p $$
                                 prefix "    " (showNicely pt)

instance (Effect p, Show n, ShowPatch p) => Show (Named n p C(x y)) where
    show = renderString . showPatch

instance (Apply p, Effect p, ShowPatch p) => ShowPatch (FL p) where
    showPatch xs = vcat (mapFL showPatch xs)
    showContextPatch s = showContextStuff s . effect -- showContextSeries
    description = vcat . mapFL description
    summary = vcat . mapFL summary
    thing x = thing (helperx x) ++ "s"
        where helperx :: FL a C(x y) -> a C(x y)
              helperx _ = undefined
    things = thing

instance (Effect p, Apply p, ShowPatch p) => ShowPatch (RL p) where
    showPatch = showPatch . reverseRL
    showContextPatch s = showContextPatch s . reverseRL
    description = description . reverseRL
    summary = summary . reverseRL
    thing = thing . reverseRL
    things = things . reverseRL

instance (Effect p, Patchy p) => Patchy (FL p)
instance (Effect p, Patchy p) => Patchy (RL p)
