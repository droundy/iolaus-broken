%  Copyright (C) 2003 David Roundy
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
module Arcs.ArgumentDefaults ( get_default_flags ) where
import Data.Maybe ( catMaybes, listToMaybe, mapMaybe )

import Arcs.Arguments ( ArcsFlag, optionFlags,
                         ArcsOption( ArcsArgOption, ArcsNoArgOption, ArcsMultipleChoiceOption ),
                         arein, isin )
import Arcs.Command ( CommandControl( Command_data ),
                        command_alloptions )
import Arcs.Help ( command_control_list )
\end{code}

\paragraph{defaults}\label{defaults}

Default values for darcs commands can be configured on a per-repository
basis by editing (and possibly creating) the \verb!.arcs-prefs/defaults!
file.  Each line of this file has the following form:
\begin{verbatim}
COMMAND FLAG VALUE
\end{verbatim}
where \verb!COMMAND! is either the name of the command to which the default
applies, or \verb!ALL! to indicate that the default applies to all commands
accepting that flag.  The \verb!FLAG! term is the name of the long argument
option without the ``\verb!--!'', i.e.\ \verb!verbose! rather than
\verb!--verbose!.  Finally, the \verb!VALUE! option can be omitted if the
flag is one such as \verb!verbose! that doesn't involve a value.
If the value has spaces in it, use single quotes, not double quotes, to surround it. 
Each line only takes one flag.  To set multiple defaults for the same
command (or for \verb!ALL! commands), use multiple lines.

Note that the use of \verb|ALL| easily can have unpredicted consequences,
especially if commands in newer versions of darcs accepts flags that they
didn't in previous versions. A command like \verb|obliterate| could be
devastating with the ``wrong'' flags (for example --all). Only use safe
flags with \verb|ALL|.

\begin{tabular}{ll}
{\tt \verb!~/.darcs/defaults!} & provides defaults for this user account \\
{\tt \verb!repo/.arcs-prefs/defaults!} & provides defaults for one project,\\
  & overrules changes per user \\
\end{tabular}

For example, if your system clock is bizarre, you could instruct darcs to
always ignore the file modification times by adding the following line to
your \verb!.arcs-prefs/defaults! file.  (Note that this would have to be
done for each repository!)
\begin{verbatim}
ALL ignore-times
\end{verbatim}

If you never want to run a test when recording to a particular repository
(but still want to do so when running
\verb'check' on that repository), and like to name
all your patches ``Stupid patch'', you could use the following:
\begin{verbatim}
record no-test
record patch-name Stupid patch
\end{verbatim}

If you would like a command to be run every time patches are recorded
in a particular repository (for example if you have one central
repository, that all developers contribute to), then you can set apply
to always run a command when apply is successful.  For example, if you
need to make sure that the files in the repository have the correct
access rights you might use the following.  There are two things
to note about using darcs this way:
\begin{itemize}
\item Without the second line you will get errors, because the sub
      process that runs apply cannot prompt interactively.
\item Whatever script is run by the post apply command should not be
      added to the repository with \verb!darcs add!; doing so would
      allow people to modify that file and then run arbitrary scripts on
      your main repository, possibly damaging or violating security.
\end{itemize}
\begin{verbatim}
apply posthook chmod -R a+r *
apply run-posthook
\end{verbatim}

Similarly, if you need a command to run automatically before darcs
performs an action you can use a prehook.  Using prehooks it could be
possible to canonicalize line endings before recording patches.

There are some options which are meant specifically for use in
\verb!.arcs-prefs/defaults!. One of them is \verb!--disable!. As the name
suggests, this option will disable every command that got it as argument. So,
if you are afraid that you could damage your repositories by inadvertent use of
a command like amend-record, add the following line to
\verb!.arcs-prefs/defaults!:
\begin{verbatim}
amend-record disable
\end{verbatim}

Also, a global preferences file can be created with the name
\verb!.darcs/defaults! in your home directory. Options present there will
be added to the repository-specific preferences.
If they conflict with repository-specific options, the repository-specific
ones will take precedence.

\begin{code}
get_default_flags :: String -> [ArcsOption] -> [ArcsFlag] -> IO [ArcsFlag]
get_default_flags com com_opts already = do
    repo_defs   <- default_content (return []) -- $ get_preflist "defaults"
    global_defs <- default_content (return []) -- $ get_global   "defaults"
    let repo_flags = get_flags_from com com_opts already repo_defs
        global_flags = get_flags_from com com_opts
                                          (already++repo_flags) global_defs
    return $ repo_flags ++ global_flags

get_flags_from :: String -> [ArcsOption] -> [ArcsFlag] -> [(String,String,String)] -> [ArcsFlag]
get_flags_from com com_opts already defs =
    options_for com_defs com_opts com_opts ++
    options_for all_defs com_opts all_opts
    where com_defs = filter (\ (c,_,_) -> c == com) defs
          all_defs = filter (\ (c,_,_) -> c == "ALL") defs
          options_for d o ao = concatMap (find_option o ao already) d
          all_opts = concatMap get_opts command_control_list
          get_opts (Command_data c) = let (o1, o2) = command_alloptions c
                                      in o1 ++ o2
          get_opts _                = []

old_flags :: [String]
old_flags = ["set-scripts-executable", "dont-set-scripts-executable"]

find_option :: [ArcsOption] -> [ArcsOption] -> [ArcsFlag] -> (String,String,String) -> [ArcsFlag]
find_option opts all_opts already (c, f, d) =
    case mapMaybe choose_option all_opts of
    [] -> if f `elem` old_flags || f `elem` map ("--"++) old_flags
          then []
          else if c == "ALL"
               then error $ "Bad default option: no command has option --"++f++"."
               else error $ "Bad default option: command '"++c++"' has no option --"++f++"."
    _ -> concat $ mapMaybe choose_option opts
    where choose_option doption | f `notElem` optionFlags doption = Nothing
          choose_option (ArcsNoArgOption _ _ o _) =
              if o `elem` already
              then Just []
              else if null d   then Just [o]
                               else error $ "Bad default option: '"++f
                                        ++"' takes no argument, but '"++d
                                        ++"' argument given."
          choose_option (ArcsArgOption _ _ o _ _) =
              if  o `isin` already
              then Just []
              else  if null d  then error $ "Bad default option: '"++f
                                        ++"' requires an argument, but no "
                                        ++"argument given."
                               else Just [o d]
          choose_option (ArcsMultipleChoiceOption os) =
              if os `arein` already then Just []
                                    else listToMaybe $ mapMaybe choose_option os
          choose_option _ = Nothing

default_content :: IO [String] -> IO [(String,String,String)]
default_content = fmap (catMaybes . map (doline.words))
    where doline (c:a:r) = Just (c, drop_dashdash a, unwords r)
          doline _ = Nothing
          drop_dashdash ('-':'-':a) = a
          drop_dashdash a = a

\end{code}

