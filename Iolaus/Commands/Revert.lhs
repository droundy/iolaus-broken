%  Copyright (C) 2002-2005 David Roundy
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

\subsection{iolaus revert}
\begin{code}
{-# LANGUAGE CPP #-}
#include "gadts.h"

module Iolaus.Commands.Revert ( revert ) where
import System.Exit ( ExitCode(..), exitWith )
import Control.Monad ( when )
import Data.List ( sort )

import Iolaus.English (englishNum, This(..), Noun(..))
import Iolaus.Command ( Command(..), nodefaults )
import Iolaus.Arguments ( Flag( All ),
                          working_repo_dir, mergeStrategy,
                        all_interactive,
                        fixSubPaths, areFileArgs )
import Iolaus.Utils ( askUser )
import Iolaus.RepoPath ( toFilePath )
import Iolaus.Patch ( Prim, invert, apply, apply_to_slurpy )
import Iolaus.Ordered ( FL(..), (:>)(..), lengthFL )
import Iolaus.SelectChanges ( with_selected_last_changes_to_files )
--import Iolaus.Commands.Unrevert ( write_unrevert )
import Iolaus.SlurpDirectory ( Slurpy )
import Iolaus.Progress ( debugMessage )
import Iolaus.Repository ( Unrecorded(..), get_unrecorded, slurp_recorded )

import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( lsfiles, diffTrees )
import Git.Helpers ( writeSlurpTree )

\end{code}
\begin{code}
revert_description :: String
revert_description =
 "Revert to the recorded version (not always reversible)."
\end{code}

\options{revert}

\haskell{revert_help} The actions of a revert may be reversed using the
unrevert command (see subsection~\ref{unrevert}).  However, if you've made
changes since the revert your mileage may vary, so please be careful.

\begin{code}
revert_help :: String
revert_help =
 "Revert is used to undo changes made to the working copy which have\n"++
 "not yet been recorded.  You will be prompted for which changes you\n"++
 "wish to undo. The last revert can be undone safely using the unrevert\n"++
 "command if the working copy was not modified in the meantime.\n"
\end{code}
\begin{code}
revert :: Command
revert = Command {command_name = "revert",
                       command_help = revert_help,
                       command_description = revert_description,
                       command_extra_args = -1,
                       command_extra_arg_help = ["[FILE or DIRECTORY]..."],
                       command_command = revert_cmd,
                       command_prereq = amInRepository,
                       command_get_arg_possibilities = lsfiles,
                       command_argdefaults = nodefaults,
                       command_advanced_options = [],
                       command_basic_options = [mergeStrategy,all_interactive,
                                                working_repo_dir]}
\end{code}
You can give revert optional arguments indicating files or directories.  If
you do so it will only prompt you to revert changes in those files or in
files in those directories.
\begin{code}
revert_cmd :: [Flag] -> [String] -> IO ()
revert_cmd opts args =
    do files <- sort `fmap` fixSubPaths opts args
       when (areFileArgs files) $
            putStrLn $ "Reverting changes in "++unwords (map show files)++"..\n"
       old <- slurp_recorded opts
       Unrecorded chs new <- get_unrecorded opts
       with_selected_last_changes_to_files "revert" opts old
           (map toFilePath files) chs $ \selected ->
               case selected of
                 _:>NilFL -> putStrLn "There are no changes to revert!"
                 _:>ch ->
                     do let theseChanges = englishNum (lengthFL ch) $
                                           This $ Noun $ "change"
                        yorn <- if All `elem` opts
                                then return "y"
                                else askUser $
                                         "Do you really want to revert " ++
                                         theseChanges "? "
                        case yorn of 'y':_ -> return ()
                                     _ -> exitWith $ ExitSuccess
                        debugMessage "about to apply inverse patch!"
                        write_unrevert new ch
                        apply (invert ch)

write_unrevert :: Slurpy C(y) -> FL Prim C(x y) -> IO ()
write_unrevert s p =
    do new <- writeSlurpTree s
       old <- apply_to_slurpy (invert p) s >>= writeSlurpTree
       diffTrees [] old new [] >>= writeFile ".git/unrevert"
\end{code}

