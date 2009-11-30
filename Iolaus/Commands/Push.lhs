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

\begin{code}
{-# LANGUAGE CPP #-}

module Iolaus.Commands.Push ( push ) where

import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import Iolaus.Command ( Command(..) )
import Iolaus.Arguments ( Flag, working_repo_dir, dryrun, test, testByDefault,
                          match_several_or_first, all_interactive, remote_repo )
import Iolaus.Repository ( push_heads, add_heads )
import Iolaus.SelectCommits ( select_commits )

import Git.Dag ( notIn, cauterizeHeads )
import Git.Plumbing ( listRemotes, heads, remoteHeads, fetchPack )
import Git.Helpers ( testCommits )
import Git.LocateRepo ( amInRepository )
#include "impossible.h"

push_description :: String
push_description =
 "Push patches from this repository to another one."

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
                                         all_interactive]++
                                        dryrun "push"++test++
                                        [working_repo_dir]}
    where deforigin _ _ [] = return ["origin"]
          deforigin _ _ xs = return xs

push_cmd :: [Flag] -> [String] -> IO ()
push_cmd opts [""] = push_cmd opts []
push_cmd opts [repodir] =
    do -- absolute '.' also taken into account by fix_filepath
       hs <- remoteHeads repodir
       if null hs then return () -- nothing to fetch
                  else fetchPack repodir -- so we can see what they've got!
       ourhs <- heads
       topush <- select_commits "push" opts (reverse $ ourhs `notIn` hs)
       when (null topush) $ do putStrLn "No commits to push!"
                               exitWith ExitSuccess
       mc <- testCommits (testByDefault opts) "Merge" (topush++hs)
       let topush' = case mc of
                       Just c | length (cauterizeHeads topush) > 1 -> [c]
                       _ -> topush
       push_heads repodir topush'
       add_heads opts topush'
push_cmd _ _ = impossible
\end{code}
