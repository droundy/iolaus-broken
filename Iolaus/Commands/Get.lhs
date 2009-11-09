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

\subsection{iolaus get}
\begin{code}
{-# LANGUAGE CPP #-}

module Iolaus.Commands.Get ( get ) where

import Iolaus.Command ( Command(..), nodefaults )
import Iolaus.Arguments ( Flag, working_repo_dir,
                        reponame, lazy, nolinks )

import Git.LocateRepo ( amNotInRepository )
import Git.Plumbing ( clone )
\end{code}
\begin{code}
get_description :: String
get_description =
 "Create a local copy of another repository."
\end{code}

\options{get}

If the remote repository and the current directory are in the same
filesystem and that filesystem supports hard links, get will create
hard links for the patch files, which means that the additional
storage space needed will be minimal.  This is \emph{very} good for
your disk usage (and for the speed of running get), so if you want
multiple copies of a repository, I strongly recommend first running
\verb!iolaus get! to get yourself one copy, and then running
\verb!iolaus get! on that copy to make any more you like.  The only
catch is that the first time you run \verb!iolaus push! or \verb!iolaus
pull!  from any of these second copies, by default they will access
your first copy---which may not be what you want.

You may specify the name of the repository created by providing a second
argument to get, which is a directory name.

\begin{code}
get_help :: String
get_help =
 "Get is used to get a local copy of a repository.\n"
\end{code}
\begin{code}
get :: Command
get = Command {command_name = "get",
                    command_help = get_help,
                    command_description = get_description,
                    command_extra_args = -1,
                    command_extra_arg_help = ["<REPOSITORY>", "[<DIRECTORY>]"],
                    command_command = get_cmd,
                    command_prereq = amNotInRepository,
                    command_get_arg_possibilities = return [],
                    command_argdefaults = nodefaults,
                    command_advanced_options = [],
                    command_basic_options = [reponame, lazy,
                                             nolinks,
                                             working_repo_dir]}
\end{code}
\begin{code}
get_cmd :: [Flag] -> [String] -> IO ()
get_cmd _ [inrepodir, outname] = clone [inrepodir, outname]
get_cmd _ [inrepodir] = clone [inrepodir]
get_cmd _ _ = fail "You must provide 'get' with either one or two arguments."

\end{code}

