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

\subsubsection{arcs show commit}
\begin{code}
module Iolaus.Commands.ShowCommit ( show_commit ) where

import Iolaus.Arguments ( IolausFlag(..), summary, working_repo_dir )
import Iolaus.Command ( IolausCommand(..), nodefaults )
import Iolaus.Printer ( putDocLnWith )
import Iolaus.ColorPrinter ( fancyPrinters )
import Iolaus.Patch ( showContextPatch, summarize )
import Iolaus.FileName ( fp2fn )

import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( parseRev, nameRevs, catCommit, CommitEntry(myParents) )
import Git.Helpers ( slurpTree,
                     mergeCommits, diffCommit, Strategy( FirstParent ) )
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
show_commit :: IolausCommand
show_commit = IolausCommand {
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
  command_basic_options = [summary, working_repo_dir] }

commit_cmd :: [IolausFlag] -> [String] -> IO ()
commit_cmd opts cs = mapM_ showc cs
    where showc c =
              do x <- parseRev c
                 commit <- catCommit x
                 putStr $ show commit
                 old <- mergeCommits FirstParent (myParents commit) >>=
                        slurpTree (fp2fn ".")
                 ch <- diffCommit FirstParent x
                 if Summary `elem` opts
                   then putDocLnWith fancyPrinters $ summarize ch
                   else putDocLnWith fancyPrinters $ showContextPatch old ch
\end{code}
