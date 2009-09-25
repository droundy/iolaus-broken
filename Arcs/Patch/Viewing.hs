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

module Arcs.Patch.Viewing ( summarize ) where

import Prelude hiding ( pi )
import Control.Monad ( liftM )
import Data.List ( sort )

import Arcs.SlurpDirectory ( Slurpy, get_slurp, get_filecontents )
import Arcs.ByteStringUtils (linesPS )
import qualified Data.ByteString as B (null)
import Arcs.FileName ( FileName, fp2fn, fn2fp )
import Arcs.Printer ( Doc, empty, vcat,
                 text, blueText, Color(Cyan,Magenta), lineColor,
                 minus, plus, ($$), (<+>), (<>),
                 prefix, renderString,
                 userchunkPS,
               )
import Arcs.Patch.Core ( Named(..) )
import Arcs.Patch.Prim ( Prim(..), isHunk, formatFileName, showPrim,
                         Effect, effect,
                         DirPatchType(..), FilePatchType(..) )
import Arcs.Patch.Patchy ( Patchy, Apply, ShowPatch(..), identity )
import Arcs.Patch.Apply ( apply_to_slurpy )
#include "impossible.h"
#include "gadts.h"
import Arcs.Ordered ( RL(..), FL(..), mapFL, reverseRL )

instance ShowPatch Prim where
    showPatch = showPrim
    showContextPatch s p@(FP _ (Hunk _ _ _)) = showContextHunk s p
    showContextPatch _ p = showPatch p
    summary = gen_summary . (:>: NilFL)
    thing _ = "change"

summarize :: Effect e => e C(x y) -> Doc
summarize = gen_summary . effect

showContextSeries :: (Apply p, ShowPatch p, Effect p) => Slurpy -> FL p C(x y) -> Doc
showContextSeries slur patches = scs slur identity patches
    where scs :: (Apply p, ShowPatch p, Effect p) => Slurpy -> Prim C(w x) -> FL p C(x y) -> Doc
          scs s pold (p:>:ps) =
              case isHunk p of
              Nothing -> showContextPatch s p $$ scs s' identity ps
              Just hp ->
                  case ps of
                  NilFL -> coolContextHunk s pold hp identity
                  (p2:>:_) ->
                      case isHunk p2 of
                      Nothing -> coolContextHunk s pold hp identity $$ scs s' hp ps
                      Just hp2 -> coolContextHunk s pold hp hp2 $$
                                  scs s' hp ps
              where s' =
                        fromJust $ apply_to_slurpy p s
          scs _ _ NilFL = empty

showContextHunk :: (Apply p, ShowPatch p, Effect p) => Slurpy -> p C(x y) -> Doc
showContextHunk s p = case isHunk p of
                        Just h -> coolContextHunk s identity h identity
                        Nothing -> showPatch p

coolContextHunk :: Slurpy -> Prim C(a b) -> Prim C(b c)
                -> Prim C(c d) -> Doc
coolContextHunk s prev p@(FP f (Hunk l o n)) next =
    case (linesPS . get_filecontents) `liftM` get_slurp f s of
    Nothing -> showPatch p -- This is a weird error...
    Just ls ->
        let numpre = case prev of
                     (FP f' (Hunk lprev _ nprev))
                         | f' == f &&
                           l - (lprev + length nprev + 3) < 3 &&
                           lprev < l ->
                             max 0 $ l - (lprev + length nprev + 3)
                     _ -> if l >= 4 then 3 else l - 1
            pre = take numpre $ drop (l - numpre - 1) ls
            numpost = case next of
                      (FP f' (Hunk lnext _ _))
                          | f' == f && lnext < l+length n+4 &&
                            lnext > l ->
                              lnext - (l+length n)
                      _ -> 3
            cleanedls = case reverse ls of
                        (x:xs) | B.null x -> reverse xs
                        _ -> ls
            post = take numpost $ drop (max 0 $ l+length o-1) cleanedls
            in blueText "hunk" <+> formatFileName f <+> text (show l)
            $$ prefix " " (vcat $ map userchunkPS pre)
            $$ lineColor Magenta (prefix "-" (vcat $ map userchunkPS o))
            $$ lineColor Cyan    (prefix "+" (vcat $ map userchunkPS n))
            $$ prefix " " (vcat $ map userchunkPS post)
coolContextHunk _ _ _ _ = impossible

gen_summary :: FL Prim C(x y) -> Doc
gen_summary p
    = vcat themoves
   $$ vcat themods
    where themods = map summ $ combine $ sort $ concat $ mapFL s p
          s :: Prim C(x y) -> [(FileName, Int, Int, Int, Bool)]
          s (FP f (Hunk _ o n)) = [(f, length o, length n, 0, False)]
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
    showContextPatch = showContextSeries
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
