%  Copyright (C) 2007 Eric Kow, 2009 David Roundy
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

\subsubsection{darcs show contents}
\begin{code}
module Grit.Commands.ShowContents ( show_contents ) where

import qualified Data.ByteString as B

import Grit.Command ( GritCommand(..), nodefaults )
import Grit.Arguments ( GritFlag, match_one,
                         working_repo_dir, fixSubPaths )
import Grit.RepoPath ( toFilePath )
import Grit.IO ( mReadFilePS )
import Grit.SlurpDirectory ( withSlurpy )
import Grit.FileName ( fp2fn )

import Git.Plumbing ( parseRev, catCommitTree )
import Git.Helpers ( slurpTree )
import Git.LocateRepo ( amInRepository )
\end{code}

\options{show contents}
\begin{code}
show_contents_description :: String
show_contents_description = "Outputs a specific version of a file."
\end{code}
\haskell{show_contents_help}
\begin{code}
show_contents_help :: String
show_contents_help =
  "Show contents can be used to display an earlier version of some file(s).\n"++
  "If you give show contents no version arguments, it displays the recorded\n"++
  "version of the file(s).\n"

show_contents :: GritCommand
show_contents = GritCommand {command_name = "contents",
                              command_help = show_contents_help,
                              command_description = show_contents_description,
                              command_extra_args = -1,
                              command_extra_arg_help
                                    = ["[FILE]..."],
                              command_command = show_contents_cmd,
                              command_prereq = amInRepository,
                              command_get_arg_possibilities = return [],
                              command_argdefaults = nodefaults,
                              command_advanced_options = [],
                              command_basic_options = [match_one, working_repo_dir]}
\end{code}

\begin{code}
show_contents_cmd :: [GritFlag] -> [String] -> IO ()
show_contents_cmd opts args =
    do path_list <- fixSubPaths opts args
       s <- parseRev "HEAD" >>= catCommitTree >>= slurpTree (fp2fn ".")
       case withSlurpy s $ mapM (mReadFilePS.fp2fn.toFilePath) path_list of
         Left e -> fail e
         Right xs -> mapM_ B.putStr $ snd xs
\end{code}
