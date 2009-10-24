%  Copyright (C) 2002-2005,2009 David Roundy
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

\subsection{iolaus pull}
\begin{code}
{-# LANGUAGE CPP #-}

module Iolaus.Commands.Pull ( pull ) where

import System.Exit ( ExitCode(..), exitWith )
import Control.Monad ( when )

import Iolaus.Command ( Command(..) )
import Iolaus.Arguments
    ( Flag, pull_conflict_options, all_interactive, repo_combinator,
      match_several_or_first,
      notest, testByDefault, mergeStrategy, working_repo_dir, remote_repo )
import Iolaus.Patch ( apply, merge )
import Iolaus.Ordered ( (:/\:)(..), (:\/:)(..) )
import Iolaus.Diff ( diff )
import Iolaus.Sealed ( Sealed(..) )
import Iolaus.Repository ( add_heads, slurp_working, slurp_recorded )
import Iolaus.SelectCommits ( select_commits )

import Git.Dag ( notIn )
import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( heads, remoteHeads, listRemotes, fetchPack )
import Git.Helpers ( test, slurpTree, mergeCommits )

pull_description :: String
pull_description =
 "Copy and apply patches from another repository to this one."
\end{code}

\options{pull}

\haskell{pull_help}
\begin{code}
pull_help :: String
pull_help =
 "Pull is used to bring changes made in another repository into the current\n"++
 "repository (that is, either the one in the current directory, or the one\n"++
 "specified with the --repodir option). Pull allows you to bring over all or\n"++
 "some of the patches that are in that repository but not in this one. Pull\n"++
 "accepts arguments, which are URLs from which to pull, and when called\n"++
 "without an argument, pull will use the repository from which you have most\n"++
 "recently either pushed or pulled.\n"

pull :: Command
pull = Command {command_name = "pull",
                command_help = pull_help,
                command_description = pull_description,
                command_extra_args = -1,
                command_extra_arg_help = ["[REPOSITORY]..."],
                command_command = pull_cmd,
                command_prereq = amInRepository,
                command_get_arg_possibilities = listRemotes,
                command_argdefaults = deforigin,
                command_advanced_options = [repo_combinator,
                                            mergeStrategy,remote_repo],
                command_basic_options = [all_interactive,
                                         pull_conflict_options,
                                         match_several_or_first]++
                                         notest++[working_repo_dir]}
    where deforigin _ _ [] = return ["origin"]
          deforigin _ _ xs = return xs

pull_cmd :: [Flag] -> [String] -> IO ()

pull_cmd opts repodirs@(_:_) =
    do -- Test to make sure we aren't trying to pull from the current repo
       when (null repodirs) $ fail "Can't pull from current repository!"
       old <- slurp_recorded opts
       Sealed work <- slurp_working
       mapM_ fetchPack repodirs
       newhs <- concat `fmap` mapM remoteHeads repodirs
       hs <- heads
       newhs' <- select_commits "pull" opts (reverse $ newhs `notIn` hs)
       when (null newhs') $ do putStrLn "No patches to pull!"
                               exitWith ExitSuccess
       Sealed newtree <- mergeCommits opts (hs++newhs')
       new <- slurpTree newtree
       case merge (diff opts old work :\/: diff opts old new) of
         Nothing ->
             do putStrLn "Unwilling to pull when it conflicts with working..."
                exitWith (ExitFailure 1)
         Just (nps :/\: _) ->
             do test (testByDefault opts) newtree
                add_heads opts newhs'
                apply nps
pull_cmd _ [] = fail "No default repository to pull from, please specify one"
\end{code}


\begin{options}
--patches, --tags
\end{options}

The \verb!--patches!, and \verb!--tags!  options can be used to select
which patches to pull, as described in subsection~\ref{selecting}.

\begin{options}
--no-test, --test
\end{options}

If you specify the \verb!--test! option, pull will run the test (if a
test exists) on a scratch copy of the repository contents prior to
actually performing the pull.  If the test fails, the pull will be
aborted.
