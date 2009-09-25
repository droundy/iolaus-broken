%  Copyright (C) 2002-2003,2009 David Roundy
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2, or (at your option)
%  any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; see the file COPYING.  If not, write to
%  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
%  Boston, MA 02110-1301, USA.

\begin{code}
{-# OPTIONS_GHC -cpp -fno-warn-orphans #-}
#include "gadts.h"
module Arcs.Patch ( Prim, Named, Patchy, apply,
                    rmfile, addfile, rmdir, adddir, move,
                    hunk, description,
                    showContextPatch, showPatch, showNicely,
                    infopatch, thing, things,
                    is_similar, is_addfile, is_hunk,
                    Effect, effect,
                    writePatch, is_adddir,
                    Invert, invert, invertFL, invertRL, identity,
                    Commute, commute, merge, list_touched_files,
                    commuteFL, commuteRL,
                    canonize, sort_coalesceFL,
                    try_to_shrink, try_shrinking_inverse,
                    apply_to_slurpy, patchcontents,
                    patch2patchinfo,
                    summary, summarize,
                    mergeFL,
                    -- from Permutations
                    module Arcs.Patch.Permutations,
                  ) where
import Arcs.Patch.Core ( Named, infopatch,
                         patch2patchinfo, patchcontents )
import Arcs.Patch.Patchy ( Patchy, writePatch, mergeFL,
                           showPatch, showNicely, showContextPatch,
                           Invert(invert, identity), invertRL, invertFL,
                           thing, things,
                           Commute(merge, commute, list_touched_files),
                           commuteFL, commuteRL, apply,
                           description, summary )
import Arcs.Patch.Permutations ( commuteWhatWeCanRL, commuteWhatWeCanFL,
                                 partitionFL, partitionRL,
                                 remove_subsequenceRL, removeFL )
import Arcs.Patch.Viewing ( summarize )
import Arcs.Patch.Apply ( apply_to_slurpy )
import Arcs.Patch.Prim ( Effect(effect),
                         Prim, canonize,
                         sort_coalesceFL,
                         rmdir, rmfile, adddir, addfile,
                         hunk, move, 
                         is_adddir, is_addfile,
                         is_hunk, is_similar,
                         try_to_shrink, try_shrinking_inverse )

instance Patchy Prim
\end{code}

\input{Arcs/Patch/Apply.lhs}
\input{Arcs/Patch/Core.lhs}
\input{Arcs/Patch/Prim.lhs}
\input{Arcs/Patch/Commute.lhs}

