%  Copyright (C) 2002-2004 David Roundy
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

\subsection{iolaus add}
\begin{code}
module Iolaus.Commands.Add ( add ) where

import Git.Plumbing ( updateindex, lsothers )
import Git.LocateRepo ( amInRepository )

import Iolaus.Command
import Iolaus.Arguments (noskip_boring, allow_problematic_filenames,
                       recursive, working_repo_dir,
                       IolausFlag, fixSubPaths,
                      )
import Iolaus.RepoPath ( toFilePath )
\end{code}

\begin{code}
add_description :: String
add_description =
 "Add one or more new files or directories."
\end{code}

\options{add}

\haskell{add_help}

\begin{code}
add_help :: String
add_help =
 "Add needs to be called whenever you add a new file or directory to your\n"++
 "project.  Of course, it also needs to be called when you first create the\n"++
 "project, to let iolaus know which files should be kept track of.\n"
\end{code}

\begin{code}
add :: IolausCommand
add = IolausCommand {command_name = "add",
                    command_help = add_help,
                    command_description = add_description,
                    command_extra_args = -1,
                    command_extra_arg_help = ["<FILE or DIRECTORY> ..."],
                    command_command = add_cmd,
                    command_prereq = amInRepository,
                    command_get_arg_possibilities = lsothers,
                    command_argdefaults = nodefaults,
                    command_advanced_options = [],
                    command_basic_options =
                    [noskip_boring, allow_problematic_filenames,
                     recursive "add contents of subdirectories",
                     working_repo_dir]}
\end{code}

Iolaus will refuse to add a file or directory that differs from an existing
one only in case.  This is because the HFS+ file system used on MacOS
treats such files as being one and the same.

You can not add symbolic links to iolaus.
If you try to do that, iolaus will refuse and print an error message.
Perhaps you want to make symbolic links \emph{to} the files in iolaus instead?

\begin{options}
--boring
\end{options}

By default iolaus will ignore all files that match any of the boring patterns.
If you want to add such a file anyway you must use the \verb!--boring! option.

\begin{code}
add_cmd :: [IolausFlag] -> [String] -> IO ()
add_cmd opts args =
 do origfiles <- map toFilePath `fmap` fixSubPaths opts args
    updateindex origfiles
\end{code}

