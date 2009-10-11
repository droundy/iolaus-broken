%  Copyright (C) 2002-2003 David Roundy
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

\subsection{arcs mv}
\begin{code}
{-# LANGUAGE CPP #-}

module Iolaus.Commands.Mv ( mv ) where

import System.Directory ( createDirectory )

import Iolaus.Command ( Command(..), nodefaults )
import Iolaus.Arguments ( Flag, working_repo_dir,
                        allow_problematic_filenames )
import Iolaus.RepoPath ( parentDir, createDirectoryIfMissing,
                       doesDirectoryExist, setCurrentDirectory )
import Iolaus.Workaround ( renameFile )

import Git.LocateRepo ( amInRepositoryDirectory )
import Git.Plumbing ( lsfiles, lssomefiles )
\end{code}

\begin{code}
mv_description :: String
mv_description =
 "Move/rename one or more files or directories."
\end{code}

\options{mv}

\haskell{mv_help} This is why ``mv'' isn't called ``move'', since it is
really almost equivalent to the unix command ``mv''.

\begin{code}
mv_help :: String
mv_help =
 "Iolaus mv needs to be called whenever you want to move files or\n"++
 "directories. Unlike remove, mv actually performs the move itself in your\n"++
 "working copy.\n"
\end{code}

\begin{code}
mv :: Command
mv = Command {command_name = "mv",
                   command_help = mv_help,
                   command_description = mv_description,
                   command_extra_args = -1,
                   command_extra_arg_help = ["<SOURCE> ... <DESTINATION>"],
                   command_command = mv_cmd,
                   command_prereq = amInRepositoryDirectory,
                   command_get_arg_possibilities = lsfiles,
                   command_argdefaults = nodefaults,
                   command_advanced_options = [],
                   command_basic_options = [allow_problematic_filenames,
                                            working_repo_dir]}
mv_cmd :: [Flag] -> [String] -> IO ()
mv_cmd _ [] = fail "You must specify at least two arguments for mv"
mv_cmd _ [_] = fail "You must specify at least two arguments for mv"
\end{code}

\begin{code}
mv_cmd _ [old,new] =
    do isd <- doesDirectoryExist new
       if isd then mvtodir old new
              else do isod <- doesDirectoryExist old
                      if isod
                         then mvdir old new
                         else mvfile old new
mv_cmd _ xs = mapM_ (`mvdir` last xs) (init xs) 

mvtodir :: FilePath -> FilePath -> IO ()
mvtodir a b =
  do xs <- lssomefiles [a]
     mapM_ (\x -> mvfile x (b++'/':x)) xs

mvdir :: FilePath -> FilePath -> IO ()
mvdir o n =
    do xs <- lssomefiles [o]
       createDirectory n
       let odir = takeWhile (/='/') o
       setCurrentDirectory odir
       mapM_ (`mvtodir` ("../"++n)) $ map (drop (1+length odir)) xs

mvfile :: FilePath -> FilePath -> IO ()
mvfile a b =
    do createDirectoryIfMissing True (parentDir b)
       renameFile a b
\end{code}


