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

\subsubsection{iolaus show commit}
\begin{code}
module Iolaus.Commands.ShowCommit ( show_commit ) where

import Iolaus.Arguments ( Flag(..), mergeStrategy,
                          summary, working_repo_dir )
import Iolaus.Command ( Command(..), nodefaults )
import Iolaus.Printer ( putDocLnWith )
import Iolaus.ColorPrinter ( fancyPrinters )
import Iolaus.Patch ( showContextPatch, summarize, invert, apply_to_slurpy )
import Iolaus.Sealed ( Sealed(Sealed), FlippedSeal(FlippedSeal) )

import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( parseRev, nameRevs, catCommit, CommitEntry(myTree) )
import Git.Helpers ( slurpTree, diffCommit )
\end{code}

\options{show commit}

\haskell{show_commit_help}

\begin{code}
show_commit_description :: String
show_commit_description = "Show a given commit."
\end{code}

\begin{code}
show_commit_help :: String
show_commit_help =
 "The commit command nicely shows a commit."
\end{code}

\begin{code}
show_commit :: Command
show_commit = Command {
  command_name = "commit",
  command_help = show_commit_help,
  command_description = show_commit_description,
  command_extra_args = -1,
  command_extra_arg_help = ["<commitish>..."],
  command_command = commit_cmd,
  command_prereq = amInRepository,
  command_get_arg_possibilities = nameRevs,
  command_argdefaults = nodefaults,
  command_advanced_options = [],
  command_basic_options = [mergeStrategy, summary, working_repo_dir] }

commit_cmd :: [Flag] -> [String] -> IO ()
commit_cmd opts cs = mapM_ showc cs
    where showc c =
              do Sealed x <- parseRev c
                 commit <- catCommit x
                 putStr $ show commit
                 FlippedSeal ch <- diffCommit opts x
                 new <- slurpTree (myTree commit)
                 let Just old = apply_to_slurpy (invert ch) new
                 if Summary `elem` opts
                   then putDocLnWith fancyPrinters $ summarize ch
                   else putDocLnWith fancyPrinters $ showContextPatch old ch
\end{code}
