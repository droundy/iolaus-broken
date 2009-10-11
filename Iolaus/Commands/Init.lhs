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

\subsection{iolaus initialize}\label{initialize}
\begin{code}
module Iolaus.Commands.Init ( initialize, initialize_cmd ) where
import Iolaus.Command ( Command(..), nodefaults )
import Iolaus.Arguments ( Flag, working_repo_dir )

import Git.LocateRepo ( amNotInRepository )
import Git.Plumbing ( gitInit )
\end{code}

\options{initialize}

\haskell{initialize_description}

\begin{code}
initialize_description :: String
initialize_description = "Initialize a new source tree as an iolus repository."
\end{code}
Call \verb|initialize| once for each project you work on. Run it from
the top level directory of the project, with the project files already
there.  \verb|initialize| will set up all the directories and files
iolaus needs in order to start keeping track of revisions for your
project.

\begin{code}
initialize_help :: String
initialize_help =
 "Call initialize once for each project you work on. Run it from the top\n"++
 "level directory of the project, with the project files already there.\n"++
 "Initialize will set up all the directories and files iolaus needs in order to\n"++
 "start keeping track of revisions for your project.\n"
\end{code}

\begin{code}
initialize :: Command
initialize = Command {command_name = "initialize",
                         command_help = initialize_help,
                         command_description = initialize_description,
                         command_extra_args = 0,
                         command_extra_arg_help = [],
                         command_prereq = amNotInRepository,
                         command_command = initialize_cmd,
                         command_get_arg_possibilities = return [],
                         command_argdefaults = nodefaults,
                         command_advanced_options = [],
                         command_basic_options = [working_repo_dir]}
\end{code}

\begin{code}
initialize_cmd :: [Flag] -> [String] -> IO ()
initialize_cmd _ _ = gitInit []
\end{code}

