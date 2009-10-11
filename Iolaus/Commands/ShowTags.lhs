%  Copyright (C) 2009 David Roundy
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

\subsubsection{arcs show tags}
\begin{code}
module Iolaus.Commands.ShowTags ( show_tags ) where

import System.Directory ( getDirectoryContents )
import Iolaus.Arguments ( Flag(..), working_repo_dir )
import Iolaus.Command ( Command(..), nodefaults )

import Git.LocateRepo ( amInRepository )
\end{code}

\options{show tags}

\haskell{show_tags_help}

\begin{code}
show_tags_description :: String
show_tags_description = "Show all tags in the repository."
\end{code}

\begin{code}
show_tags_help :: String
show_tags_help =
 "The tags command outputs a list of all tags in the repository."
\end{code}

\begin{code}
show_tags :: Command
show_tags = Command {
  command_name = "tags",
  command_help = show_tags_help,
  command_description = show_tags_description,
  command_extra_args = 0,
  command_extra_arg_help = [],
  command_command = tags_cmd,
  command_prereq = amInRepository,
  command_get_arg_possibilities = return [],
  command_argdefaults = nodefaults,
  command_advanced_options = [],
  command_basic_options = [working_repo_dir] }

tags_cmd :: [Flag] -> [String] -> IO ()
tags_cmd _ _ =
    getDirectoryContents ".git/refs/tags" >>= mapM_ putStrLn
\end{code}
