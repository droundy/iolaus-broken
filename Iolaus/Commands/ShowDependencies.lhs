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

\subsubsection{iolaus show dependencies}
\begin{code}
module Iolaus.Commands.ShowDependencies ( show_dependencies ) where

import Iolaus.Arguments ( Flag(..), mergeStrategy,
                          summary, working_repo_dir )
import Iolaus.Command ( Command(..), nodefaults )
import Iolaus.Printer ( putDocLnWith )
import Iolaus.ColorPrinter ( fancyPrinters )
import Iolaus.Patch ( summarize )
import Iolaus.Sealed ( Sealed(Sealed), FlippedSeal(FlippedSeal) )
import Iolaus.Repository ( slurp_working )

import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( Hash, Commit,
                      heads, nameRevs, catCommit )
import Git.Helpers ( writeSlurpTree, diffCommit, simplifyParents )
\end{code}

\options{show dependencies}

\haskell{show_dependencies_help}

\begin{code}
show_dependencies_description :: String
show_dependencies_description = "Show a given dependencies."
\end{code}

\begin{code}
show_dependencies_help :: String
show_dependencies_help =
 "The dependencies command nicely shows a dependencies."
\end{code}

\begin{code}
show_dependencies :: Command
show_dependencies = Command {
  command_name = "dependencies",
  command_help = show_dependencies_help,
  command_description = show_dependencies_description,
  command_extra_args = -1,
  command_extra_arg_help = ["[commitish]..."],
  command_command = dependencies_cmd,
  command_prereq = amInRepository,
  command_get_arg_possibilities = nameRevs,
  command_argdefaults = nodefaults,
  command_advanced_options = [],
  command_basic_options = [mergeStrategy, summary, working_repo_dir] }

dependencies_cmd :: [Flag] -> [String] -> IO ()
dependencies_cmd opts _ =
    do Sealed s <- slurp_working
       t <- writeSlurpTree s
       hs <- heads
       (hs', _) <- simplifyParents opts hs t
       mapM_ (showc opts) hs'

showc :: [Flag] -> Sealed (Hash Commit) -> IO ()
showc opts (Sealed x) =
    do commit <- catCommit x
       putStr $ show commit
       if Summary `elem` opts
           then do FlippedSeal ch <- diffCommit opts x
                   putDocLnWith fancyPrinters $ summarize ch
           else return ()
\end{code}
