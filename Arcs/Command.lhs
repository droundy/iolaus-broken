%  Copyright (C) 2002,2003,2005 David Roundy
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
module Arcs.Command ( CommandControl( Command_data, Hidden_command, Group_name ),
                       ArcsCommand( ArcsCommand, command_name,
                                     command_help, command_description,
                                     command_basic_options, command_advanced_options,
                                     command_command,
                                     command_prereq,
                                     command_extra_arg_help,
                                     command_extra_args,
                                     command_argdefaults,
                                     command_get_arg_possibilities,
                                     SuperCommand,
                                     command_sub_commands ),
                       command_alias, command_stub,
                       command_options, command_alloptions,
                       disambiguate_commands, CommandArgs(..),
                       get_command_help, get_command_mini_help,
                       get_subcommands,
                       usage, subusage, chomp_newline,
                       extract_commands,
                       super_name,
                       nodefaults,
                       loggers,
                     ) where

import System.Console.GetOpt( OptDescr, usageInfo )

import Data.List ( sort, isPrefixOf )
import Arcs.Arguments ( ArcsFlag, ArcsOption, disable, help,
                         any_verbosity, posthook_cmd,
                         prehook_cmd, option_from_darcsoption )
import Arcs.RepoPath ( AbsolutePath, rootDirectory )
import Arcs.Utils ( putStrLnError )
import Arcs.Printer ( Doc, putDocLn )
\end{code}

The general format of an arcs command is
\begin{verbatim}
% arcs COMMAND OPTIONS ARGUMENTS ...
\end{verbatim}
Here \verb|COMMAND| is a command such as \verb|add| or \verb|record|, which of
course may have one or more arguments.  Options have the form
\verb!--option! or \verb!-o!, while arguments vary from command to
command.  There are many options which are common to a number of different
commands, which will be summarized here.

If you wish, you may use any unambiguous beginning of a command name as a
shortcut: for \verb!arcs record!, you could type \verb!arcs recor! or
\verb!arcs rec!, but not \verb!arcs re! since that could be confused with
\verb!arcs replace!, \verb!arcs revert! and \verb!arcs remove!.

In some cases, \verb|COMMAND| actually consists of two words, a
super-command and a subcommand.  For example, the ``display the
manifest'' command has the form \verb|arcs query manifest|.

\paragraph{Command overview}

Not all commands modify the ``patches'' of your repository (that
is, the named patches which other users can pull); some commands only
affect the copy of the source tree you're working on (your ``working
directory''), and some affect both. This table summarizes what you should
expect from each one and will hopefully serve as guide when you're having
doubts about which command to use.

\begin{center}
\footnotetext[1]{But it affects the repository and working directory targeted
  by the push}
\footnotetext[2]{As for the other end, see apply}
\begin{tabular}{|c|c|c|}
\hline
affects & patches & working directory\\
\hline
record & yes & no\\
\hline
unrecord & yes & no\\
\hline
rollback & yes & yes\\
\hline
revert & no & yes\\
\hline
unrevert & no & yes\\
\hline
pull & yes & yes\\
\hline
obliterate & yes & yes\\
\hline
apply & yes & yes\\
\hline
push\footnote{But it affects the repository and working directory targeted by
the push} & no & no\\
\hline
send\footnote{As for the other end, see apply} & no & no\\
\hline
put\footnote{Creates a new repository} & no & no\\
\hline
\end{tabular}
\end{center}

\begin{code}
extract_commands, extract_hidden_commands :: [CommandControl] -> [ArcsCommand]
extract_commands cs = concatMap (\x -> case x of { Command_data cmd_d -> [cmd_d]; _ -> []}) cs
extract_hidden_commands cs = concatMap (\x -> case x of { Hidden_command cmd_d -> [cmd_d]; _ -> []}) cs
\end{code}

\input{Arcs/Arguments.lhs}

\begin{code}
data CommandControl = Command_data ArcsCommand
                    | Hidden_command ArcsCommand
                    | Group_name String

data ArcsCommand =
    ArcsCommand {command_name, command_help, command_description :: String,
                  command_extra_args :: Int,
                  command_extra_arg_help :: [String],
                  command_command :: [ArcsFlag] -> [String] -> IO (),
                  command_prereq :: [ArcsFlag] -> IO (Either String ()),
                  command_get_arg_possibilities :: IO [String],
                  command_argdefaults :: [ArcsFlag] -> AbsolutePath -> [String] -> IO [String],
                  command_basic_options :: [ArcsOption],
                  command_advanced_options :: [ArcsOption]}
  | SuperCommand {command_name, command_help, command_description :: String,
                  command_prereq :: [ArcsFlag] -> IO (Either String ()),
                  command_sub_commands :: [CommandControl]}

command_alloptions :: ArcsCommand -> ([ArcsOption], [ArcsOption])
command_alloptions ArcsCommand { command_basic_options = opts1
                                , command_advanced_options = opts2 }
    = (opts1 ++ [disable, help],
       any_verbosity ++ opts2 ++
                [posthook_cmd,prehook_cmd])

--  Supercommands cannot be disabled.
command_alloptions SuperCommand { } = ([help],[])

--  Obtain options suitable as input to
--  System.Console.Getopt, including the --disable option (which is
--  not listed explicitly in the ArcsCommand definitions).
command_options :: AbsolutePath -> ArcsCommand -> ([OptDescr ArcsFlag], [OptDescr ArcsFlag])
command_options cwd c = (convert basic, convert advanced)
 where (basic, advanced) = command_alloptions c
       convert = concatMap (option_from_darcsoption cwd)

nodefaults :: [ArcsFlag] -> AbsolutePath -> [String] -> IO [String]
nodefaults _ _ xs = return xs

get_subcommands :: ArcsCommand -> [CommandControl]
get_subcommands c@(SuperCommand {}) = command_sub_commands c
get_subcommands _ = []

command_alias :: String -> ArcsCommand -> ArcsCommand
command_alias n c =
  c { command_name = n
    , command_description = "Alias for `arcs " ++ command_name c ++ "'."
    , command_help = "The `arcs " ++ n ++ "' command is an alias for " ++
                     "`arcs " ++ command_name c ++ "'.\n" ++
                     command_help c
    }

command_stub :: String -> String -> String -> ArcsCommand -> ArcsCommand
command_stub n h d c =
  c { command_name = n
    , command_help = h
    , command_description = d
    , command_command = \_ _ -> putStr h
    }
\end{code}

\begin{code}
usage :: [CommandControl] -> String
usage cs = "Usage: arcs COMMAND ...\n\nCommands:\n" ++
           usage_helper cs ++ "\n" ++
           "Use 'arcs COMMAND --help' for help on a single command.\n" ++
           "Use 'arcs --version' to see the arcs version number.\n" ++
           "Use 'arcs --exact-version' to get the exact version of this arcs instance.\n" ++
           "Use 'arcs help --match' for help on patch matching.\n\n" ++
           "Check bug reports at http://bugs.arcs.net/\n"

subusage :: ArcsCommand -> String
subusage super =
    (usageInfo
     ("Usage: arcs "++command_name super++" SUBCOMMAND ... " ++
      "\n\n"++ command_description super++
      "\n\nSubcommands:\n" ++ usage_helper (get_subcommands super) ++ "\nOptions:")
     (option_from_darcsoption rootDirectory help))
    ++ "\n" ++ command_help super

usage_helper :: [CommandControl] -> String
usage_helper [] = ""
usage_helper (Hidden_command _:cs) = usage_helper cs
usage_helper ((Command_data c):cs) = "  "++pad_spaces (command_name c) 15 ++
                      chomp_newline (command_description c)++"\n"++usage_helper cs
usage_helper ((Group_name n):cs) = n ++ "\n" ++ usage_helper cs

chomp_newline :: String -> String
chomp_newline "" = ""
chomp_newline s = if last s == '\n' then init s else s

pad_spaces :: String -> Int -> String
pad_spaces s n = s ++ replicate (n - length s) ' '

super_name :: Maybe ArcsCommand -> String
super_name Nothing  = ""
super_name (Just x) = command_name x ++ " "

get_command_mini_help :: Maybe ArcsCommand -> ArcsCommand -> String
get_command_mini_help msuper cmd =
  get_command_help_core msuper cmd ++
  "\n\nSee arcs help "
  ++ (maybe "" (\c -> command_name c ++ " ") msuper)
  ++ command_name cmd ++ " for details."

get_command_help :: Maybe ArcsCommand -> ArcsCommand -> String
get_command_help msuper cmd =
    unlines (reverse basicR)
    ++ (if null advanced then ""
        else "\nAdvanced options:\n" ++ unlines (reverse advancedR))
    ++ "\n" ++ command_help cmd
    where -- we could just call usageInfo twice, but then the advanced
          -- options might not line up with the basic ones (no short flags)
          (advancedR, basicR) =
             splitAt (length advanced) $ reverse $ lines combinedUsage
          combinedUsage = usageInfo
            (get_command_help_core msuper cmd ++ subcommands ++ "\n\nOptions:")
            (basic ++ advanced)
          (basic, advanced) = command_options rootDirectory cmd
          subcommands =
            case msuper of
            Nothing -> case get_subcommands cmd of
                       [] -> []
                       s  -> "\n\nSubcommands:\n" ++ (usage_helper s)
            -- we don't want to list subcommands if we're already specifying them
            Just _  -> ""

get_command_help_core :: Maybe ArcsCommand -> ArcsCommand -> String
get_command_help_core msuper cmd =
    "Usage: arcs "++super_name msuper++command_name cmd++
    " [OPTION]... " ++ unwords args_help ++
    "\n"++ command_description cmd
    where args_help = case cmd of
            (ArcsCommand _ _ _ _ _ _ _ _ _ _ _) ->
              command_extra_arg_help cmd
            _ -> []
\end{code}

\begin{code}
data CommandArgs = CommandOnly      ArcsCommand
                 | SuperCommandOnly ArcsCommand
                 | SuperCommandSub  ArcsCommand ArcsCommand

-- Parses an arcs command line with potentially abbreviated commands
disambiguate_commands :: [CommandControl] -> String -> [String]
                      -> Either String (CommandArgs, [String])
disambiguate_commands allcs cmd args =
 case extract cmd allcs of
 Left e -> Left e
 Right c -> case (get_subcommands c, args) of
            ([], _)         -> Right (CommandOnly c, args)
            (_ ,[])         -> Right (SuperCommandOnly c, args)
            (subcs, (a:as)) -> case extract a subcs of
                               Left _   -> Right (SuperCommandOnly c, args)
                               Right sc -> Right (SuperCommandSub c sc, as)

extract :: String -> [CommandControl] -> Either String ArcsCommand
extract cmd cs =
 case [ c | c <- extract_commands cs, cmd `isPrefixOf` command_name c ] ++
      [ h | h <- extract_hidden_commands cs,    cmd == command_name h ] of
   []  -> Left $ "No such command '" ++ cmd ++ "'\n"
   [c] -> Right c
   cs' -> Left $ "Ambiguous command...\n\n" ++
                    "The command '"++cmd++"' could mean one of:\n" ++
                    unwords (sort $ map command_name cs')
\end{code}

\begin{code}
-- | Output functions equivalent to (putStrLn, hPutStrLn stderr, putDocLn)
loggers :: [ArcsFlag] -> ( String -> IO ()
                          , String -> IO ()
                          , Doc -> IO ())
loggers _ = (putStrLn, putStrLnError, putDocLn)
\end{code}
