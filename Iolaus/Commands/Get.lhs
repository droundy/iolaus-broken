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

\begin{code}
module Iolaus.Commands.Get ( get ) where

import Iolaus.Command ( Command(..), nodefaults )
import Iolaus.Arguments ( Flag, working_repo_dir, reponame )

import Git.LocateRepo ( amNotInRepository )
import Git.Plumbing ( clone )

get_description :: String
get_description =
 "Create a local copy of another repository."

get_help :: String
get_help =
 "Get is used to get a local copy of a repository.\n"

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
               command_basic_options = [reponame, working_repo_dir]}

get_cmd :: [Flag] -> [String] -> IO ()
get_cmd _ [inrepodir, outname] = clone [inrepodir, outname]
get_cmd _ [inrepodir] = clone [inrepodir]
get_cmd _ _ = fail "You must provide 'get' with either one or two arguments."
\end{code}

You may specify the name of the repository created by providing a second
argument to get, which is a directory name.
