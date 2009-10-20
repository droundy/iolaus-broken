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

\subsection{iolaus push}
\begin{code}
{-# LANGUAGE CPP #-}

module Iolaus.Commands.Push ( push ) where

import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import Iolaus.Workaround ( getCurrentDirectory )
import Iolaus.Command ( Command(..) )
import Iolaus.Arguments ( Flag, working_repo_dir, summary,
                          match_several_or_first, all_interactive, remote_repo )
import Iolaus.Repository ( push_heads )
import Iolaus.SelectCommits ( select_commits )

import Git.Dag ( notIn )
import Git.Plumbing ( listRemotes, heads, remoteHeads )
import Git.LocateRepo ( amInRepository )
#include "impossible.h"

push_description :: String
push_description =
 "Push patches from this repository to another one."
\end{code}

\options{push}
\haskell{push_help}
\begin{code}
push_help :: String
push_help =
 "Push is the opposite of pull.  Push allows you to copy changes from the\n"++
 "current repository into another repository.\n"

push :: Command
push = Command {command_name = "push",
                command_help = push_help,
                command_description = push_description,
                command_extra_args = -1,
                command_extra_arg_help = ["[REPOSITORY]"],
                command_command = push_cmd,
                command_prereq = amInRepository,
                command_get_arg_possibilities = listRemotes,
                command_argdefaults = deforigin,
                command_advanced_options = [remote_repo],
                command_basic_options = [match_several_or_first,
                                         all_interactive,
                                         summary, working_repo_dir]}
    where deforigin _ _ [] = return ["origin"]
          deforigin _ _ xs = return xs

push_cmd :: [Flag] -> [String] -> IO ()
push_cmd opts [""] = push_cmd opts []
push_cmd opts [repodir] =
    do -- Test to make sure we aren't trying to push to the current repo
       here <- getCurrentDirectory
       when (repodir == here) $
            fail "Cannot push from repository to itself."
       -- absolute '.' also taken into account by fix_filepath
       hs <- remoteHeads repodir
       ourhs <- heads
       topush <- select_commits "push" opts (reverse $ ourhs `notIn` hs)
       when (null topush) $ do putStrLn "No patches to push!"
                               exitWith ExitSuccess       
       push_heads repodir topush
push_cmd _ _ = impossible
\end{code}
