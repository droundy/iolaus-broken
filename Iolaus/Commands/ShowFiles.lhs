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

\subsubsection{iolaus show files}
\label{show-files}
\begin{code}
{-# LANGUAGE CPP #-}
#include "gadts.h"

module Iolaus.Commands.ShowFiles ( show_files, show_manifest ) where
import Iolaus.Arguments ( Flag(..), working_repo_dir,
                        files, directories, nullFlag )
import Iolaus.Command ( Command(..), nodefaults, command_alias )
import Iolaus.SlurpDirectory ( Slurpy, list_slurpy,
                             list_slurpy_files, list_slurpy_dirs )
import Iolaus.Sealed ( Sealed(..), mapSealM )

import Git.Plumbing ( parseRev, catCommitTree )
import Git.Helpers ( slurpTree )
import Git.LocateRepo ( amInRepository )
\end{code}

\options{show files}

\haskell{show_files_help}

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
show_files :: Command
show_files = Command {
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
  command_basic_options = [files, directories, nullFlag,
                          working_repo_dir] }

show_manifest :: Command
show_manifest = command_alias "manifest" show_files {
  command_command = manifest_cmd to_list_manifest
}

to_list_files, to_list_manifest :: [Flag] -> Slurpy C(x) -> [FilePath]
to_list_files opts =
    files_dirs (NoFiles `notElem` opts) (NoDirectories `notElem` opts)
to_list_manifest opts =
    files_dirs (NoFiles `notElem` opts) (Directories `elem` opts)

files_dirs :: Bool -> Bool -> Slurpy C(x) -> [FilePath]
files_dirs False False = \_ -> []
files_dirs False True  = list_slurpy_dirs
files_dirs True  False = list_slurpy_files
files_dirs True  True  = list_slurpy

manifest_cmd :: (FORALL(x) [Flag] -> Slurpy C(x) -> [FilePath])
             -> [Flag] -> [String] -> IO ()
manifest_cmd to_list opts _ =
    do Sealed s <- parseRev "HEAD" >>= mapSealM catCommitTree
                   >>= mapSealM slurpTree
       mapM_ output $ to_list opts s
    where output_null name = do { putStr name ; putChar '\0' }
          output = if NullFlag `elem` opts then output_null else putStrLn
\end{code}
