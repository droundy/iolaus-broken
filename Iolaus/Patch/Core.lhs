%  Copyright (C) 2002-2003 David Roundy
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


\section{Patch relationships}

\begin{code}
{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{-# LANGUAGE CPP #-}
-- , GADTs, PatternGuards #-}

#include "gadts.h"

module Iolaus.Patch.Core ( Named(..), infopatch, n_fn,
                         patch2patchinfo, patchcontents ) where

import Prelude hiding ( pi )
import Iolaus.FileName ( fn2fp, fp2fn, norm_path )
import Iolaus.Patch.Patchy ( Patchy )
import Iolaus.Patch.Prim ( Effect(effect, effectRL) )

data Named n p C(x y) where
    NamedP :: !n -> !(p C(x y)) -> Named n p C(x y)

instance Effect p => Effect (Named n p) where
    effect (NamedP _ p) = effect p
    effectRL (NamedP _ p) = effectRL p

infopatch :: Patchy p => n -> p C(x y) -> Named n p C(x y)
infopatch n p = NamedP n p

patch2patchinfo :: Named n p C(x y) -> n
patch2patchinfo (NamedP i _) = i

patchcontents :: Named n p C(x y) -> p C(x y)
patchcontents (NamedP _ p) = p

n_fn :: FilePath -> FilePath
n_fn = fn2fp . norm_path . fp2fn
\end{code}
