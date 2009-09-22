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

\subsection{darcs unrecord}
\label{unrecord}
\begin{code}
{-# LANGUAGE CPP #-}

module Arcs.Commands.Unrecord ( unrecord ) where

import Arcs.Command ( ArcsCommand(..), nodefaults )
import Arcs.Arguments ( ArcsFlag, working_repo_dir,
                        match_several_or_last )
import Arcs.Utils ( askUser )

import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( updateref, revListHashes, revList, RevListOption(..) )
#include "gadts.h"
\end{code}
\begin{code}
unrecord_description :: String
unrecord_description =
 "Remove recorded patches without changing the working copy."
\end{code}

\options{unrecord}

\haskell{unrecord_help}

Unrecord can be thought of as undo-record.
If a record is followed by an unrecord, everything looks like before
the record; all the previously unrecorded changes are back, and can be
recorded again in a new patch. The unrecorded patch however is actually
removed from your repository, so there is no way to record it again to get
it back.\footnote{The patch file itself is not actually deleted, but its
context is lost, so it cannot be reliably read---your only choice would be
to go in by hand and read its contents.}.

If you want to remove
the changes from the working copy too (where they otherwise will show
up as unrecorded changes again), you'll also need to \verb!darcs revert!.
To do unrecord and revert in one go, you can use \verb!darcs obliterate!.

If you don't revert after unrecording, then the changes made by the
unrecorded patches are left in your working tree.  If these patches are
actually from another repository, interaction (either pushes or pulls) with
that repository may be massively slowed down, as darcs tries to cope with
the fact that you appear to have made a large number of changes that
conflict with those present in the other repository.  So if you really want
to undo the result of a \emph{pull} operation, use obliterate! Unrecord is
primarily intended for when you record a patch, realize it needs just one
more change, but would rather not have a separate patch for just that one
change.

\newcommand{\pullwarning}[1]{
\textbf{WARNING:} #1 should not be run when there is a possibility
that another user may be pulling from the same repository.  Attempting to do so
may cause repository corruption.}

\pullwarning{Unrecord}

\begin{options}
--from-match, --from-patch, --from-tag, --last
\end{options}

Usually you only want to unrecord the latest changes,
and almost never would you want to unrecord changes before a tag---you
would have to have unrecorded the tag as well to do that.
Therefore, and for efficiency, darcs only prompts you for the latest patches,
after some optimal tag.

If you do want to unrecord more patches in one go,
there are the \verb!--from! and \verb!--last! options
to set the earliest patch selectable to unrecord.

\begin{options}
--matches, --patches, --tags, --no-deps
\end{options}

The \verb!--patches!, \verb!--matches!, \verb!--tags!, and \verb!--no-deps!
options can be used to select which patches to unrecord, as described in
subsection~\ref{selecting}.

With these options you can specify
what patch or patches to be prompted for by unrecord.
This is especially useful when you want to unrecord patches with dependencies,
since all the dependent patches (but no others) will be included in the choices.
Or if you use \verb!--no-deps! you won't be asked about patches that can't be
unrecorded due to depending patches.

Selecting patches can be slow, so darcs cuts the search at the last
optimized tag. Use the \verb!--from! or \verb!--last! options to search
more or fewer patches.

\begin{code}
unrecord_help :: String
unrecord_help =
 "Unrecord does the opposite of record in that it makes the changes from\n"++
 "patches active changes again which you may record or revert later.  The\n"++
 "working copy itself will not change.\n"++
 "Beware that you should not use this command if you are going to\n"++
 "re-record the changes in any way and there is a possibility that\n"++
 "another user may have already pulled the patch.\n"
\end{code}
\begin{code}
unrecord :: ArcsCommand
unrecord = ArcsCommand {command_name = "unrecord",
                         command_help = unrecord_help,
                         command_description = unrecord_description,
                         command_extra_args = 0,
                         command_extra_arg_help = [],
                         command_command = unrecord_cmd,
                         command_prereq = amInRepository,
                         command_get_arg_possibilities = return [],
                         command_argdefaults = nodefaults,
                         command_advanced_options = [],
                         command_basic_options = [match_several_or_last,
                                                  working_repo_dir]}
\end{code}
\begin{code}
unrecord_cmd :: [ArcsFlag] -> [String] -> IO ()
unrecord_cmd _ _ =
    do revs <- revListHashes
       case revs of
         _:p:_ ->
             do m <- revList [MaxCount 1, MediumPretty, RelativeDate]
                putStrLn m
                yorn <- askUser "Shall I unrecord this patch? "
                case yorn of
                  'y':_ -> updateref "refs/heads/master" p
                  _ -> putStrLn "Unrecord cancelled."
         _ -> putStrLn "Sorry, you need two patches to do an unrecord."
\end{code}

