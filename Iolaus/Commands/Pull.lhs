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

\begin{code}
module Iolaus.Commands.Pull ( pull ) where

import System.Exit ( ExitCode(..), exitWith )
import Control.Monad ( when, filterM )
import Data.List ( union, intersect )

import Iolaus.Command ( Command(..) )
import Iolaus.Arguments
    ( Flag(All, NoAllowConflicts, Intersection), pull_conflict_options, verify,
      all_interactive, repo_combinator, match_several_or_first, dryrun,
      notest, testByDefault, working_repo_dir, remote_repo )
import Iolaus.Patch ( apply, merge, mergeNamed, infopatch, patchcontents,
                      invert )
import Iolaus.Ordered ( (:/\:)(..), (:\/:)(..), (+>+), mapFL_FL )
import Iolaus.Diff ( diff )
import Iolaus.Sealed ( Sealed(..), mapSealM )
import Iolaus.Repository ( add_heads, slurp_working, slurp_recorded )
import Iolaus.SelectCommits ( select_commits, isMerge )
import Iolaus.Utils ( askUser )
import Iolaus.IO ( runTolerantly )

import Git.Dag ( notIn )
import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( heads, remoteHeads, remoteTagNames, listRemotes )
import Git.Helpers ( testCommits, slurpTree, mergeCommits, verifyCommit )

pull_description :: String
pull_description =
 "Copy and apply patches from another repository to this one."

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
                command_advanced_options = [repo_combinator, remote_repo],
                command_basic_options = [all_interactive,
                                         pull_conflict_options,
                                         match_several_or_first]++
                                         notest++verify++dryrun "pull"++
                                         [working_repo_dir]}
    where deforigin _ _ [] = return ["origin"]
          deforigin _ _ xs = return xs

pull_cmd :: [Flag] -> [String] -> IO ()

pull_cmd opts repodirs@(_:_) =
    do let putInfo = if Quiet `elem` opts then const (return ()) else putStrLn
       old <- slurp_recorded
       Sealed work <- slurp_working
       hs <- heads
       allnewhs <- map (`notIn` hs) `fmap` mapM remoteHeads repodirs
       let newhs = reverse $ if Intersection `elem` opts
                             then foldl1 intersect allnewhs
                             else foldl1 union allnewhs
       mapM_ (verifyCommit opts) newhs
       merges <- filterM isMerge newhs
       newhs' <- select_commits "pull" opts (reverse $ newhs `notIn` hs)
       when (null newhs') $ do putInfo "No patches to pull!"
                               exitWith ExitSuccess
       Sealed new <- mergeCommits (hs++newhs') >>= mapSealM slurpTree
       let workp = diff opts old work
           newp = diff opts old new
       Sealed nps <-
           case merge (workp :\/: newp) of
           Nothing | All `elem` opts ->
            do putStrLn "Unwilling to pull -a when it conflicts with working..."
               exitWith $ ExitFailure 1
           Nothing | NoAllowConflicts `elem` opts ->
            do putStrLn "Pull conflicts with working!"
               exitWith $ ExitFailure 1
           Nothing ->
             do putStrLn "This conflicts with working..."
                yn <- askUser "Are you sure you want to pull? "
                case yn of
                  'y':_ ->
                      case mergeNamed
                           [Sealed $ mapFL_FL (infopatch "working") workp,
                            Sealed $ mapFL_FL (infopatch $ head repodirs) newp]
                      of
                        Sealed xs -> return $ Sealed $ invert workp +>+
                                                       mapFL_FL patchcontents xs
                  _ -> do putStrLn "Pull cancelled."
                          exitWith (ExitFailure 1)
           Just (nps :/\: _) -> return (Sealed nps)
       let newhs'' = newhs' ++ filter nicemerge merges
               where nicemerge x = [x] `notIn` newhs' == [x]
       testCommits (testByDefault opts)
                       (unwords $ "Merge":repodirs) (hs++newhs'')
       add_heads opts newhs''
       runTolerantly $ apply nps
       tns <- concat `fmap` mapM remoteTagNames repodirs
       let writeTag (h,n) | take 10 n == "refs/tags/" =
                              writeFile (".git/"++n) (show h)
           writeTag _ = return ()
       mapM_ writeTag tns
pull_cmd _ [] = fail "No default repository to pull from, please specify one"
\end{code}

\begin{options}
--patches, --tags
\end{options}

The `--patches`, and `--tags`  options can be used to select
which patches to pull, as described in subsection~\ref{selecting}.

\begin{options}
--no-test, --test
\end{options}

If you specify the `--test` option, pull will run the test (if a
test exists) on a scratch copy of the repository contents prior to
actually performing the pull.  If the test fails, the pull will be
aborted.
