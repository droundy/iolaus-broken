%  Copyright (C) 2002-2004 David Roundy
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
module Arcs.Commands.Show ( show_command ) where

import Arcs.Command ( ArcsCommand(..), CommandControl(Command_data) )
import Arcs.Commands.ShowAuthors ( show_authors )
import Arcs.Commands.ShowContents ( show_contents )
import Arcs.Commands.ShowFiles ( show_files, show_manifest )
import Arcs.Commands.ShowTags ( show_tags )
import Git.LocateRepo ( amInRepository )

show_description :: String
show_description = "Show repository information."

show_help :: String
show_help =
 "Use the --help option with the subcommands to obtain help for\n"++
 "subcommands (for example, \"arcs show files --help\").\n"

show_command :: ArcsCommand
show_command = SuperCommand {
                 command_name = "show",
                 command_help = show_help,
                 command_description = show_description,
                 command_prereq = amInRepository,
                 command_sub_commands = [Command_data show_contents,
                                         Command_data show_files,
                                         Command_data show_manifest,
                                         Command_data show_authors,
                                         Command_data show_tags]
               }
\end{code}

\subsection{arcs show}

The show command provides access to several subcommands which can be
used to investigate the state of a repository.

\input{Arcs/Commands/ShowAuthors.lhs}
\input{Arcs/Commands/ShowContents.lhs}
\input{Arcs/Commands/ShowFiles.lhs}
\input{Arcs/Commands/ShowTags.lhs}

