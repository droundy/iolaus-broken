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

\subsection{darcs revert}
\begin{code}
module Arcs.Commands.Revert ( revert ) where
import System.Exit ( ExitCode(..), exitWith )
import Control.Monad ( when )
import Data.List ( sort )

import Arcs.Patch.English (englishNum, This(..), Noun(..))
import Arcs.Command ( ArcsCommand(..), nodefaults )
import Arcs.Arguments ( ArcsFlag( All ),
                        working_repo_dir,
                        all_interactive,
                        fixSubPaths, areFileArgs )
import Arcs.Utils ( askUser )
import Arcs.RepoPath ( toFilePath )
import Arcs.Patch ( invert, apply )
import Arcs.Ordered ( FL(..), (:>)(..), lengthFL )
import Arcs.SelectChanges ( with_selected_last_changes_to_files )
--import Arcs.Commands.Unrevert ( write_unrevert )
import Arcs.Diff ( unsafeDiff )
import Arcs.FileName ( fp2fn )
import Arcs.Progress ( debugMessage )

import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( lsfiles, updateindex, writetree,
                      catCommitTree, parseRev )
import Git.Helpers ( slurpTree )

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
revert :: ArcsCommand
revert = ArcsCommand {command_name = "revert",
                       command_help = revert_help,
                       command_description = revert_description,
                       command_extra_args = -1,
                       command_extra_arg_help = ["[FILE or DIRECTORY]..."],
                       command_command = revert_cmd,
                       command_prereq = amInRepository,
                       command_get_arg_possibilities = lsfiles,
                       command_argdefaults = nodefaults,
                       command_advanced_options = [],
                       command_basic_options = [all_interactive,
                                               working_repo_dir]}
\end{code}
You can give revert optional arguments indicating files or directories.  If
you do so it will only prompt you to revert changes in those files or in
files in those directories.
\begin{code}
revert_cmd :: [ArcsFlag] -> [String] -> IO ()
revert_cmd opts args =
    do files <- sort `fmap` fixSubPaths opts args
       when (areFileArgs files) $
            putStrLn $ "Reverting changes in "++unwords (map show files)++"..\n"
       fs <- lsfiles
       updateindex fs
       new <- writetree >>= slurpTree (fp2fn ".")
       old <- parseRev "HEAD" >>= catCommitTree >>= slurpTree (fp2fn ".")
       let unrevertchs (_:>NilFL) = putStrLn "There are no changes to revert!"
           unrevertchs (_:>ch) =
               do let theseChanges = englishNum (lengthFL ch) $
                                     This $ Noun $ "change"
                  yorn <- if All `elem` opts
                          then return "y"
                          else askUser $ "Do you really want to revert " ++
                               theseChanges "? "
                  case yorn of ('y':_) -> return ()
                               _ -> exitWith $ ExitSuccess
                  debugMessage "about to apply inverse patch!"
                  apply (invert ch)
       with_selected_last_changes_to_files "revert" opts old
           (map toFilePath files) (unsafeDiff [] old new) unrevertchs
\end{code}

