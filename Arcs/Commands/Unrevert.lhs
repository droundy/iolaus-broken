%  Copyright (C) 2003-2005,2009 David Roundy
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

\subsection{arcs unrevert}\label{unrevert}
\begin{code}
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Arcs.Commands.Unrevert ( unrevert ) where

import System.Directory ( removeFile )

import Arcs.Command ( ArcsCommand(..), nodefaults )
import Arcs.Arguments ( ArcsFlag, working_repo_dir, all_interactive )

import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( gitApply )

unrevert_description :: String
unrevert_description =
 "Undo the last revert (may fail if changes after the revert)."
\end{code}

\options{unrevert}

\haskell{unrevert_help}
\begin{code}
unrevert_help :: String
unrevert_help =
 "Unrevert is used to undo the results of a revert command. It is only\n"++
 "guaranteed to work properly if you haven't made any changes since the\n"++
 "revert was performed.\n"
\end{code}
The command makes a best effort to merge the unreversion with any changes
you have since made.  In fact, unrevert should even work if you've recorded
changes since reverting.
\begin{code}
unrevert :: ArcsCommand
unrevert = ArcsCommand {command_name = "unrevert",
                         command_help = unrevert_help,
                         command_description = unrevert_description,
                         command_extra_args = 0,
                         command_extra_arg_help = [],
                         command_command = unrevert_cmd,
                         command_prereq = amInRepository,
                         command_get_arg_possibilities = return [],
                         command_argdefaults = nodefaults,
                         command_advanced_options = [],
                         command_basic_options = [all_interactive,
                                                  working_repo_dir]}
\end{code}
\begin{code}
unrevert_cmd :: [ArcsFlag] -> [String] -> IO ()
unrevert_cmd _ _ = do gitApply ".git/unrevert"
                      removeFile ".git/unrevert"
\end{code}
