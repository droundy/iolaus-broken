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


\begin{code}
{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
-- , TypeOperators #-}

#include "gadts.h"

module Iolaus.Patch.Commute ( )
       where

import Iolaus.Patch.Patchy ( Commute(..), Invert(..) )
import Iolaus.Patch.Core ( Named(..) )
#include "impossible.h"
import Iolaus.Ordered ( (:/\:)(..), (:\/:)(..), (:>)(..),
                      MyEq(..) )

--import Iolaus.ColorPrinter ( traceDoc )
--import Iolaus.Printer ( greenText )

instance MyEq p => MyEq (Named n p) where
    NamedP _ x =\/= NamedP _ y = x =\/= y
    NamedP _ x =/\= NamedP _ y = x =/\= y

instance Commute p => Commute (Named n p) where
    commute (NamedP n1 p1 :> NamedP n2 p2) =
        do (p2' :> p1') <- commute (p1 :> p2)
           return (NamedP n2 p2' :> NamedP n1 p1')
    merge (NamedP n1 p1 :\/: NamedP n2 p2)
        = case merge (p1 :\/: p2) of
          (p2' :/\: p1') -> NamedP n2 p2' :/\: NamedP n1 p1'
    list_touched_files (NamedP _ p) = list_touched_files p

instance Invert p => Invert (Named n p) where
    invert (NamedP n p)  = NamedP n (invert p)
    identity = impossible
    sloppyIdentity (NamedP _ p) = sloppyIdentity p

\end{code}
