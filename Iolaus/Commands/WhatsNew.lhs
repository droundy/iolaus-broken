%  Copyright (C) 2002-2004,2009 David Roundy
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

\subsection{iolaus whatsnew}
\label{whatsnew}
\begin{code}
{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Iolaus.Commands.WhatsNew ( whatsnew ) where

import Iolaus.Command ( Command(..), nodefaults )
import Iolaus.Arguments ( Flag(Summary), mergeStrategy,
                          working_repo_dir, summary )
import Iolaus.Patch ( showContextPatch, summarize )
import Iolaus.Printer ( putDocLnWith )
import Iolaus.ColorPrinter ( fancyPrinters )
import Iolaus.Repository ( get_recorded_and_unrecorded, Unrecorded(..) )

import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( lsfiles )
\end{code}

\options{whatsnew}

\haskell{whatsnew_description}
\begin{code}
whatsnew_description :: String
whatsnew_description = "Display unrecorded changes in the working copy."
\end{code}
\haskell{whatsnew_help} \verb!iolaus whatsnew! will return a non-zero value if
there are no changes, which can be useful if you just want to see in a
script if anything has been modified.  If you want to see some context
around your changes, you can use the \verb!-u! option, to get output
similar to the unidiff format.

\begin{code}
whatsnew_help :: String
whatsnew_help =
 "whatsnew gives you a view of what changes you've made in your working\n"++
 "copy that haven't yet been recorded.  The changes are displayed in\n"++
 "iolaus patch format. Note that --look-for-adds implies --summary usage.\n"
\end{code}

\begin{code}
whatsnew :: Command
whatsnew = Command {command_name = "whatsnew",
                         command_help = whatsnew_help,
                         command_description = whatsnew_description,
                         command_extra_args = -1,
                         command_extra_arg_help = ["[FILE or DIRECTORY]..."],
                         command_command = whatsnew_cmd,
                         command_prereq = amInRepository,
                         command_get_arg_possibilities = lsfiles,
                         command_argdefaults = nodefaults,
                         command_advanced_options = [],
                         command_basic_options = [summary, mergeStrategy,
                                                  working_repo_dir]}
\end{code}

\begin{code}
whatsnew_cmd :: [Flag] -> [String] -> IO ()
whatsnew_cmd opts _ =
    do (old, Unrecorded chs _) <- get_recorded_and_unrecorded opts
       if Summary `elem` opts
          then putDocLnWith fancyPrinters $ summarize chs
          else putDocLnWith fancyPrinters $ showContextPatch old chs
\end{code}

If you give one or more file or directory names as an argument to
\verb!whatsnew!, iolaus will output only changes to those files or to files in
those directories.
