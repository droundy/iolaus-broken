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
module Iolaus.Commands.Unrecord ( unrecord ) where

import Iolaus.Command ( Command(..), nodefaults )
import Iolaus.Arguments ( Flag(RecordFor), working_repo_dir, modifySafely,
                          all_interactive, match_several_or_last )
import Iolaus.Repository ( decapitate )
import Iolaus.SelectCommits ( select_last_commits )

import Git.Dag ( notIn )
import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( heads, remoteHeads )

unrecord_description :: String
unrecord_description =
 "Remove recorded patches without changing the working copy."

unrecord_help :: String
unrecord_help =
 "Unrecord does the opposite of record in that it makes the changes from\n"++
 "patches active changes again which you may record or revert later.  The\n"++
 "working copy itself will not change.\n"++
 "Beware that you should not use this command if you are going to\n"++
 "re-record the changes in any way and there is a possibility that\n"++
 "another user may have already pulled the patch.\n"

unrecord :: Command
unrecord = Command {command_name = "unrecord",
                         command_help = unrecord_help,
                         command_description = unrecord_description,
                         command_extra_args = 0,
                         command_extra_arg_help = [],
                         command_command = unrecord_cmd,
                         command_prereq = amInRepository,
                         command_get_arg_possibilities = return [],
                         command_argdefaults = nodefaults,
                         command_advanced_options = [],
                         command_basic_options = [modifySafely,
                                                  match_several_or_last,
                                                  all_interactive,
                                                  working_repo_dir]}

unrecord_cmd :: [Flag] -> [String] -> IO ()
unrecord_cmd opts _ =
    do rf <- concat `fmap` mapM remoteHeads [c | RecordFor c <- opts]
       hs0 <- heads
       toremove <- select_last_commits "unrecord" opts (hs0 `notIn` rf)
       decapitate opts toremove
\end{code}

Unrecord can be thought of as undo-record.  If a record is followed by
an unrecord, everything looks like before the record; all the
previously unrecorded changes are back, and can be recorded again in a
new patch.

If you want to remove the changes from the working copy too (where
they otherwise will show up as unrecorded changes again), you'll also
need to `iolaus revert`.

Unrecord should not be run when there is a possibility that another
user may be pulling from the same repository.  Attempting to do so may
cause severe confusion.
