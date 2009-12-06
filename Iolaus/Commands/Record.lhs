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

#include "gadts.h"

module Iolaus.Commands.Record ( record, get_log ) where
import Control.Exception ( handleJust, Exception( ExitException ) )
import Control.Monad ( when )
import Data.List ( sort, isPrefixOf )
import System.Exit ( exitWith, exitFailure, ExitCode(..) )
import System.IO ( hPutStrLn )

import Iolaus.Lock ( readBinFile, writeBinFile, world_readable_temp,
                   appendToFile, removeFileMayNotExist )
import Iolaus.Command ( Command(..), nodefaults )
import Iolaus.Arguments ( Flag( PromptLongComment, NoEditLongComment,
                                Quiet, EditLongComment, RmLogFile,
                                LogFile, PatchName, All,
                                DeltaDebugWorkingSubset ),
                        working_repo_dir, commitApproach,
                        fixSubPaths, testByDefault, sign,
                        ask_long_comment, recordDeltaDebug,
                        all_interactive, notest,
                        author, patchname_option,
                        rmlogfile, logfile )
import Iolaus.Utils ( askUser, promptYorn, edit_file )
import Iolaus.RepoPath ( FilePathLike, toFilePath )
import Iolaus.Patch ( Prim, summarize, apply_to_slurpy )
import Iolaus.Printer ( ($$), text, hPutDocLn, wrap_text )
import Iolaus.SelectChanges ( with_selected_changes_to_files )
import Iolaus.Ordered ( (:>)(..), FL(NilFL) )
import Iolaus.Global ( debugMessage )
import Iolaus.Repository ( get_recorded_and_unrecorded, Unrecorded(..),
                           add_heads )
import Iolaus.Sealed ( Sealed(Sealed) )
import Iolaus.DeltaDebug ( largestPassingSet )

import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( lsfiles, heads )
import Git.Helpers ( testCommits, testMessage, commitTreeNicely,
                     writeSlurpTree, simplifyParents )

record :: Command
record = Command {command_name = "record",
                       command_help = record_help,
                       command_description = record_description,
                       command_extra_args = -1,
                       command_extra_arg_help = ["[FILE or DIRECTORY]..."],
                       command_command = record_cmd,
                       command_prereq = amInRepository,
                       command_get_arg_possibilities = lsfiles,
                       command_argdefaults = nodefaults,
                       command_advanced_options = [logfile, rmlogfile],
                       command_basic_options = [patchname_option, author]++
                                               notest++recordDeltaDebug++
                                               [commitApproach, all_interactive,
                                                sign, ask_long_comment,
                                                working_repo_dir]}

record_cmd :: [Flag] -> [String] -> IO ()
record_cmd opts args = do
    check_name_is_not_option opts
    files <- sort `fmap` fixSubPaths opts args
    handleJust only_successful_exits (\_ -> return ()) $ do
    (old, Unrecorded allchs0 _) <- get_recorded_and_unrecorded
    Sealed allchs <-
        if DeltaDebugWorkingSubset `elem` opts
        then do t <- writeSlurpTree old -- FIXME this issssss stupid
                xs :> _ <- largestPassingSet t allchs0
                return $ Sealed xs
        else return $ Sealed allchs0
    with_selected_changes_to_files "record" opts old (map toFilePath files)
                                   allchs $ \ (ch:>_) ->
        do debugMessage "have finished selecting changes..."
           case ch of
             NilFL -> do putStrLn "No changes selected!"
                         exitWith ExitSuccess
             _ -> return ()
           newtree <- apply_to_slurpy ch old >>= writeSlurpTree
           (name, my_log, _) <- get_log opts Nothing ch
                              (world_readable_temp "iolaus-record")
           hs <- heads
           (hs', Sealed newtree') <-
               simplifyParents (testByDefault opts) hs newtree
           testedby <- testMessage (testByDefault opts)
           let -- FIXME join with Signed-off-by:
               cleanup ("":"":r) = cleanup ("":r)
               cleanup (a:b) = a : cleanup b
               cleanup [] = []
               message = (unlines $ cleanup $ name:my_log++testedby)
           com <- commitTreeNicely opts newtree' hs' message
           -- we'll first run the test on the commit in its
           -- "primitive" context...
           debugMessage "Testing on \"canonical\" tree..."
           testCommits (testByDefault opts) "Testing" [Sealed com]
           -- now let's just check that the merged version
           -- actually passes the tests...
           debugMessage "Testing on \"current\" tree..."
           testCommits (testByDefault opts) "Merge" (Sealed com:hs)
           debugMessage "Recording the new commit..."
           add_heads opts [Sealed com]
           putStrLn ("Finished recording patch '"++ name ++"'")

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

record_description :: String
record_description =
 "Save changes in the working copy to the repository as a patch."

record_help :: String
record_help = show $ wrap_text 80 $
 "Record is used to name a set of changes and record the patch to the "++
 "repository."

data PName = FlagPatchName String | PriorPatchName String | NoPatchName

get_log :: [Flag] -> Maybe (String, [String]) -> FL Prim C(x y) -> IO String
        -> IO (String, [String], Maybe String)
get_log opts m_old changes make_log = gl opts
    where patchname_specified = patchname_helper opts
          patchname_helper (PatchName n:_) | take 4 n == "TAG " = FlagPatchName $ '.':n
                                           | otherwise          = FlagPatchName n
          patchname_helper (_:fs) = patchname_helper fs
          patchname_helper [] = case m_old of Just (p,_) -> PriorPatchName p
                                              Nothing    -> NoPatchName
          default_log = case m_old of
                          Nothing    -> []
                          Just (_,l) -> l
          gl (LogFile f:fs) =
              do -- round 1 (patchname)
                 mlp <- lines `fmap` readBinFile f `catch` (\_ -> return [])
                 firstname <- case (patchname_specified, mlp) of
                                (FlagPatchName  p, []) -> return p
                                (_, p:_)               -> return p -- logfile trumps prior!
                                (PriorPatchName p, []) -> return p
                                (NoPatchName, [])      -> prompt_patchname True
                 -- round 2
                 append_info f firstname
                 when (EditLongComment `elem` fs) $ do edit_file f
                                                       return ()
                 (name, thelog, _) <- read_long_comment f firstname
                 when (RmLogFile `elem` opts) $ removeFileMayNotExist f
                 return (name, thelog, Nothing)
          gl (EditLongComment:_) =
                  case patchname_specified of
                    FlagPatchName  p -> actually_get_log p
                    PriorPatchName p -> actually_get_log p
                    NoPatchName      -> prompt_patchname True >>= actually_get_log
          gl (NoEditLongComment:_) =
                  case patchname_specified of
                    FlagPatchName  p
                        | Just ("",_) <- m_old ->
                                       return (p, default_log, Nothing) -- rollback -m
                    FlagPatchName  p -> return (p, default_log, Nothing) -- record (or amend) -m
                    PriorPatchName p -> return (p, default_log, Nothing) -- amend
                    NoPatchName      -> do p <- prompt_patchname True -- record
                                           return (p, [], Nothing)
          gl (PromptLongComment:fs) =
                  case patchname_specified of
                    FlagPatchName p -> prompt_long_comment p -- record (or amend) -m
                    _               -> gl fs
          gl (_:fs) = gl fs
          gl [] = case patchname_specified of
                    FlagPatchName  p -> return (p, default_log, Nothing)  -- record (or amend) -m
                    PriorPatchName "" -> prompt_patchname True >>= prompt_long_comment
                    PriorPatchName p -> return (p, default_log, Nothing)
                    NoPatchName -> prompt_patchname True >>= prompt_long_comment
          prompt_patchname retry =
            do n <- askUser "What is the patch name? "
               if n == "" || take 4 n == "TAG "
                  then if retry then prompt_patchname retry
                                else fail "Bad patch name!"
                  else return n
          prompt_long_comment oldname =
            do yorn <- promptYorn "Do you want to add a long comment?"
               if yorn == 'y' then actually_get_log oldname
                              else return (oldname, [], Nothing)
          actually_get_log p = do logf <- make_log
                                  writeBinFile logf $ unlines $ p : default_log
                                  append_info logf p
                                  edit_file logf
                                  read_long_comment logf p
          read_long_comment :: FilePathLike p => p -> String -> IO (String, [String], Maybe p)
          read_long_comment f oldname =
              do t <- (lines.filter (/='\r')) `fmap` readBinFile f
                 case t of [] -> return (oldname, [], Just f)
                           (n:ls) -> return (n, takeWhile
                                             (not.(eod `isPrefixOf`)) ls,
                                             Just f)
          append_info f oldname =
              do fc <- readBinFile f
                 appendToFile f $ \h ->
                     do case fc of
                          _ | null (lines fc) -> hPutStrLn h oldname
                            | last fc /= '\n' -> hPutStrLn h ""
                            | otherwise       -> return ()
                        hPutDocLn h $ text eod
                            $$ text ""
                            $$ wrap_text 75
                               ("Place the long patch description above the "++
                                eod++
                                " marker.  The first line of this file "++
                                "will be the patch name.")
                            $$ text ""
                            $$ text "This patch contains the following changes:"
                            $$ text ""
                            $$ summarize changes

eod :: String
eod = "***END OF DESCRIPTION***"

only_successful_exits :: Exception -> Maybe ()
only_successful_exits (ExitException ExitSuccess) = Just ()
only_successful_exits _ = Nothing
\end{code}

If you provide one or more files or directories as additional arguments
to record, you will only be prompted to changes in those files or
directories.

Each patch is given a name, which typically would consist of a brief
description of the changes.  This name is later used to describe the patch.
The name must fit on one line (i.e. cannot have any embedded newlines).  If
you have more to say, stick it in the log.

Finally, each changeset should have a full log (which may be empty).  This
log is for detailed notes which are too lengthy to fit in the name.  If you
answer that you do want to create a comment file, iolaus will open an editor
so that you can enter the comment in.

\begin{options}
--logfile
\end{options}

If you wish, you may specify the patch name and log using the
`--logfile` flag.  If you do so, the first line of the specified file
will be taken to be the patch name, and the remainder will be the "long
comment".  This feature can be especially handy if you have a test that
fails several times on the record (thus aborting the record), so you don't
have to type in the long comment multiple times. The file's contents will
override the `--patch-name` option.

\begin{options}
--cauterize-all, --record-for, --commute-past
\end{options}

Describe this please.

\begin{options}
--no-test,  --test,  --test-parents
\end{options}

If you configure iolaus to run a test suite, iolaus will run this test on the
recorded repository to make sure it is valid.  Iolaus first creates a pristine
copy of the source tree (in a temporary directory), then it runs the test,
using its return value to decide if the record is valid.  If it is not valid,
the record will be aborted.  This is a handy way to avoid making stupid
mistakes.  It also can be
tediously slow, so there is an option (`--no-test`) to skip the test.

You can also use `--test-parents` if iolaus is to fast
for you, which will cause record to seek out the minimal context in
which the new patch will pass the test this is currently *very*
slow, as it doesn't do any bisection at all.

\begin{options}
--interactive, --all
\end{options}

By default, `record` works interactively. Probably the only thing
you need to know about using this is that you can press `?` at
the prompt to be shown a list of the rest of the options and what they
do. The rest should be clear from there.  The opposite is
`--all`, which causes iolaus not to prompt you, but simply to
record all changes.
