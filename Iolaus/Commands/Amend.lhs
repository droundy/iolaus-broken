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
{-# LANGUAGE CPP, PatternGuards #-}

module Iolaus.Commands.Amend ( amend_record ) where

import Control.Monad ( when )
import Data.List ( sort )
import System.Exit ( exitWith, exitFailure, ExitCode(..) )

import Iolaus.Lock ( world_readable_temp )
import Iolaus.Command ( Command(..), nodefaults )
import Iolaus.Arguments ( Flag( Quiet, PatchName, All, RecordFor,
                                DeltaDebugWorkingSubset ),
                        working_repo_dir, commitApproach,
                        fixSubPaths, testByDefault,
                        ask_long_comment, recordDeltaDebug,
                        all_interactive, notest,
                        author, patchname_option,
                        rmlogfile, logfile )
import Iolaus.Utils ( promptYorn )
import Iolaus.RepoPath ( FilePathLike, toFilePath )
import Iolaus.Patch ( apply_to_slurpy )
import Iolaus.Printer ( wrap_text )
import Iolaus.SelectChanges ( with_selected_changes_to_files )
import Iolaus.SelectCommits ( select_commit )
import Iolaus.Ordered ( (:>)(..), FL(NilFL) )
import Iolaus.Global ( debugMessage )
import Iolaus.Repository ( get_recorded_and_unrecorded, Unrecorded(..),
                           add_heads, decapitate )
import Iolaus.Commands.Record ( get_log )
import Iolaus.Sealed ( Sealed(Sealed), unseal, mapSealM )
import Iolaus.DeltaDebug ( largestPassingSet )

import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( lsfiles, heads, catCommit, myMessage, remoteHeads )
import Git.Helpers ( testCommits, testMessage, commitTreeNicely,
                     writeSlurpTree, simplifyParents )
import Git.Dag ( parents, notIn )

amend_record_description :: String
amend_record_description =
 "Amend a recent commit."

amend_record_help :: String
amend_record_help = show $ wrap_text 80 $
 "Amend-record is used to amend a commit."

amend_record :: Command
amend_record = Command {command_name = "amend-record",
                        command_help = amend_record_help,
                        command_description = amend_record_description,
                        command_extra_args = -1,
                        command_extra_arg_help = ["[FILE or DIRECTORY]..."],
                        command_command = amend_record_cmd,
                        command_prereq = amInRepository,
                        command_get_arg_possibilities = lsfiles,
                        command_argdefaults = nodefaults,
                        command_advanced_options = [logfile, rmlogfile],
                        command_basic_options = [patchname_option, author]++
                                                notest++recordDeltaDebug++
                                                [commitApproach,
                                                 all_interactive,
                                                 ask_long_comment,
                                                 working_repo_dir]}

amend_record_cmd :: [Flag] -> [String] -> IO ()
amend_record_cmd opts args = do
    check_name_is_not_option opts
    files <- sort `fmap` fixSubPaths opts args
    rf <- concat `fmap` mapM remoteHeads [c | RecordFor c <- opts]
    hs0 <- heads
    toamend <- select_commit "amend" opts (hs0 `notIn` rf)
    (old, Unrecorded allchs0 _) <- get_recorded_and_unrecorded opts
    Sealed allchs <-
        if DeltaDebugWorkingSubset `elem` opts
        then do t <- writeSlurpTree old -- FIXME this issssss stupid
                xs :> _ <- largestPassingSet t allchs0
                return $ Sealed xs
        else return $ Sealed allchs0
    with_selected_changes_to_files "add" opts old (map toFilePath files)
                                   allchs $ \ (ch:>_) ->
        do debugMessage "have finished selecting changes..."
           case ch of
             NilFL -> do putStrLn "No changes selected!"
                         exitWith ExitSuccess
             _ -> return ()
           newtree <- apply_to_slurpy ch old >>= writeSlurpTree
           Sealed oldc <- catCommit `mapSealM` toamend
           let line1:restl = lines $ myMessage oldc
               clean (x:xs)
                   | take 4 x == "Test" =
                       dropWhile null xs
                   | otherwise = x:xs
               clean [] = []
               oldmsg = (line1,reverse $ clean $ reverse restl)
           (name, my_log, _) <- get_log opts (Just oldmsg) ch
                              (world_readable_temp "iolaus-amend")
           hs <- ((unseal parents toamend++).filter (/=toamend))
                 `fmap` heads
           (hs', Sealed newtree') <- simplifyParents opts hs newtree
           testedby <- testMessage (testByDefault opts)
           let -- FIXME join with Signed-off-by:
               cleanup ("":"":r) = cleanup ("":r)
               cleanup (a:b) = a : cleanup b
               cleanup [] = []
               message = (unlines $ cleanup $ name:my_log++testedby)
           com <- commitTreeNicely newtree' hs' message
           -- we'll first run the test on the commit in its
           -- "primitive" context...
           debugMessage "Testing on \"canonical\" tree..."
           testCommits (testByDefault opts) "Testing" [Sealed com]
           -- now let's just check that the merged version
           -- actually passes the tests...
           debugMessage "Testing on \"current\" tree..."
           testCommits (testByDefault opts) "Merge" (Sealed com:hs)
           debugMessage "Recording the new commit..."
           decapitate opts [toamend]
           add_heads opts [Sealed com]
           putStrLn ("Finished amending patch '"++ name ++"'")

 -- check that what we treat as the patch name is not accidentally a command
 -- line flag
check_name_is_not_option :: [Flag] -> IO ()
check_name_is_not_option opts = do
    let putInfo = if Quiet `elem` opts then const (return ()) else putStrLn
        patchNames = [n | PatchName n <- opts]
    when (length patchNames == 1) $ do
        let n = head patchNames
            oneLetterName = length n == 1 || (length n == 2 && head n == '-')
        if (oneLetterName && not (elem All opts))
            then do
                let keepAsking = do
                    yorn <- promptYorn ("You specified " ++ show n ++ " as the patch name. Is that really what you want?")
                    case yorn of 
                        'y' -> return ()
                        'n' -> do
                                   putInfo "Okay, aborting the record."
                                   exitFailure
                        _   -> keepAsking
                keepAsking
            else return ()
\end{code}

Amend is much like record, but modifies an already-recorded commit.
It refuses, however, to modify a commit that has already been pushed
to the repository specified with `--record-for`.

If you provide one or more files or directories as additional arguments
to amend, you will only be prompted to changes in those files or
directories.
