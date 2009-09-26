%  Copyright (C) 2005 Florian Weimer, 2009 David Roundy
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

\subsubsection{darcs show files}
\label{show-files}
\begin{code}
{-# LANGUAGE CPP #-}
module Arcs.Commands.ShowFiles ( show_files, show_manifest ) where
import Arcs.Arguments ( ArcsFlag(..), working_repo_dir,
                        files, directories, pending, nullFlag )
import Arcs.Command ( ArcsCommand(..), nodefaults, command_alias )
import Arcs.SlurpDirectory ( Slurpy, list_slurpy,
                             list_slurpy_files, list_slurpy_dirs )
import Arcs.FileName ( fp2fn )

import Git.Plumbing ( parseRev, catCommitTree )
import Git.Helpers ( slurpTree )
import Git.LocateRepo ( amInRepository )
\end{code}

\options{show files}

\haskell{show_files_help}

By default (and if the \verb!--pending! option is specified),
the effect of pending patches on the repository is taken into account.
In other words, if you add a file using {\tt darcs add}, it
immediately appears in the output of {\tt query manifest}, even if it
is not yet recorded.  If you specify the \verb!--no-pending! option,
{\tt query manifest} will only list recorded files (and directories).

The \verb!--files! and \verb!--directories! options control whether
files and directories are included in the output.  The
\verb!--no-files!  and \verb!--no-directories! options have the
reverse effect.  The default is to include files, but not directories.

If you specify the \verb!--null! option, the file names are written to
standard output in unescaped form, and separated by ASCII NUL bytes.
This format is suitable for further automatic processing (for example,
using \verb!xargs -0!).

\begin{code}
show_files_description :: String
show_files_description = "Show version-controlled files in the working copy."
\end{code}

\begin{code}
show_files_help :: String
show_files_help =
 "The files command lists the version-controlled files in the\n"++
 "working copy.  The similar manifest command, lists the same\n"++
 "files, excluding any directories.\n"
\end{code}

\begin{code}
show_files :: ArcsCommand
show_files = ArcsCommand {
  command_name = "files",
  command_help = show_files_help,
  command_description = show_files_description,
  command_extra_args = 0,
  command_extra_arg_help = [],
  command_command = manifest_cmd to_list_files,
  command_prereq = amInRepository,
  command_get_arg_possibilities = return [],
  command_argdefaults = nodefaults,
  command_advanced_options = [],
  command_basic_options = [files, directories, pending, nullFlag,
                          working_repo_dir] }

show_manifest :: ArcsCommand
show_manifest = command_alias "manifest" show_files {
  command_command = manifest_cmd to_list_manifest
}

to_list_files, to_list_manifest :: [ArcsFlag] -> Slurpy -> [FilePath]
to_list_files    opts = files_dirs (NoFiles `notElem` opts) (NoDirectories `notElem` opts)
to_list_manifest opts = files_dirs (NoFiles `notElem` opts) (Directories `elem` opts)

files_dirs :: Bool -> Bool -> Slurpy -> [FilePath]
files_dirs False False = \_ -> []
files_dirs False True  = list_slurpy_dirs
files_dirs True  False = list_slurpy_files
files_dirs True  True  = list_slurpy

manifest_cmd :: ([ArcsFlag] -> Slurpy -> [FilePath]) -> [ArcsFlag] -> [String] -> IO ()
manifest_cmd to_list opts _ =
    do s <- parseRev "HEAD" >>= catCommitTree >>= slurpTree (fp2fn ".")
       mapM_ output $ to_list opts s
    where output_null name = do { putStr name ; putChar '\0' }
          output = if NullFlag `elem` opts then output_null else putStrLn
\end{code}
