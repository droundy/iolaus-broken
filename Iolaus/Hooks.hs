{- Copyright (C) 2002-2005,2008 David Roundy

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; see the file COPYING.  If not, write to
  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
  Boston, MA 02110-1301, USA. -}

module Iolaus.Hooks ( run_posthook, run_prehook ) where

import Iolaus.Utils ( withCurrentDirectory )
import System.Exit ( ExitCode(..) )
import Control.Monad ( when )
import System.Process.Redirects ( system )

import Iolaus.RepoPath ( AbsolutePath )
import Iolaus.Arguments ( IolausFlag( Quiet ),
                        get_posthook_cmd, get_prehook_cmd )
import Iolaus.Progress ( debugMessage )
import System.IO ( hPutStrLn, stderr )

run_posthook :: [IolausFlag] -> AbsolutePath -> IO ExitCode
run_posthook opts repodir = do let ph = get_posthook_cmd opts
                               withCurrentDirectory repodir $ run_hook opts "Posthook" ph

run_prehook :: [IolausFlag] -> AbsolutePath -> IO ExitCode
run_prehook opts repodir = do let ph = get_prehook_cmd opts
                              withCurrentDirectory repodir $ run_hook opts "Prehook" ph

run_hook :: [IolausFlag] -> String -> Maybe String -> IO ExitCode
run_hook _ _ Nothing = return ExitSuccess
run_hook opts cname (Just command) =
    do debugMessage $ "About to run "++cname++": " ++ command
       ec <- system command
       when (Quiet `notElem` opts) $
         if ec == ExitSuccess
         then putStrLn $ cname++" ran successfully."
         else hPutStrLn stderr $ cname++" failed!"
       return ec
