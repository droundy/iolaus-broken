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
module Iolaus.Commands.SicHercules ( sic_hercules ) where

import System.Exit ( ExitCode(..), exitWith )

import Iolaus.Command ( Command(..) )
import Iolaus.Arguments ( Flag( RecordFor ), working_repo_dir, modifySafely,
                          all_interactive, match_several_or_last )
import Iolaus.Repository ( decapitate )
import Iolaus.SelectCommits ( select_last_commits )
import Iolaus.Printer ( wrap_text )
import Iolaus.Utils ( askUser )
import Iolaus.English (englishNum, This(..), Noun(..))

import Git.Dag ( notIn )
import Git.LocateRepo ( amInRepository )
import Git.Plumbing ( heads, remoteHeads )

sic_hercules_description :: String
sic_hercules_description =
 "Remove commits from a remote repository."

sic_hercules_help :: String
sic_hercules_help = show $ wrap_text 80 $
 "sic-hercules is a violent act by a violent man.  It involves travelling "++
 "to a remote repository and hewing off selected heads that are not present "++
 "on the tame local repository.  This is done with a club, not with a sword "++
 "or scythe as modern man would use.  Because Heracles is a *real* man.  "++
 "None of this uppity technology for *him*!"

sic_hercules :: Command
sic_hercules = Command {command_name = "sic-hercules",
                        command_help = sic_hercules_help,
                        command_description = sic_hercules_description,
                        command_extra_args = 1,
                        command_extra_arg_help = [],
                        command_command = sic_hercules_cmd,
                        command_prereq = amInRepository,
                        command_get_arg_possibilities = return [],
                        command_argdefaults = deforigin,
                        command_advanced_options = [],
                        command_basic_options = [modifySafely,
                                                 match_several_or_last,
                                                 all_interactive,
                                                 working_repo_dir]}
    where deforigin _ _ [] = return ["origin"]
          deforigin _ _ xs = return xs

sic_hercules_cmd :: [Flag] -> [String] -> IO ()
sic_hercules_cmd opts [repo] =
    do r <- remoteHeads repo
       rf <- concat `fmap` mapM remoteHeads [c | RecordFor c <- opts]
       hs0 <- heads
       toremove <-select_last_commits "sic Hercules on" opts (r`notIn`(rf++hs0))
       let theseCommits = englishNum (length toremove) $
                          This $ Noun $ "commit"
       yorn<-askUser$ "Do you really want to sic Hercules on "++theseCommits"? "
       case yorn of 'y':_ -> return ()
                    _ -> do putStrLn "Cancelled destructive rage."
                            exitWith $ ExitSuccess
       decapitate opts toremove
sic_hercules_cmd _ _ = fail "impossible case in sic_hercules_cmd"
\end{code}

Sometimes you've push something to a remote server that just has no
business being there.  If you've got `ssh` access, you can log onto
that computer and just unrecord the offensive commit.  If this is a
security issue, there is more to be done, and you should consult a git
guru.  However, if it's simply a foolish patch, then you're good to go?

But what if you're using a service such as GitHub, and don't have
shell access? In that case you've got to employ some sort of
destructive agent.  Iolaus won't do that for you... he's a nice
chariot driver.  Fortunately, he's got this uncle who's pretty brutal,
and when you've got a dirty job to be done, he's your man.  You should
*see* how he cleaned out the stables of King Augeas!

Note that since Iolaus is still (nominally) in charge, even this
command won't delete commits that are still present locally.

Note also that `--record-for` is particularly useful when used with
`--sic-hercules`.  A common use case may be when you have a remote
 --scratch repository for proposing commits for a trunk repository.
 --In this case, you will want to run

    iolaus all --config-default --record-for trunk

and `sic-hercules` will only prompt you for patches that haven't been
pulled into the main trunk.
