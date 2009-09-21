-- Copyright (C) 2003 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.


{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- , DeriveDataTypeable #-}

module Arcs.Exec ( system, exec_interactive,
                    withoutNonBlock,
                    Redirects, Redirect(..),
                    ExecException(..)
                  ) where

import Data.Typeable ( Typeable )

#ifndef WIN32
import Control.Exception ( bracket )
import System.Posix.Env ( setEnv, getEnv, unsetEnv )
import System.Posix.IO ( queryFdOption, setFdOption, FdOption(..), stdInput )
import System.IO ( stdin )
#else
import Control.Exception ( catchJust, Exception(IOException) )
import Data.List ( isInfixOf )
#endif

import System.Exit ( ExitCode (..) )
#ifndef HAVE_REDIRECTS
import System.Cmd ( system )
#else
import System.Process.Redirects ( system )
#endif
import Arcs.Progress ( withoutProgress )

{-
   A redirection is a three-tuple of values (in, out, err).
   The most common values are:

     AsIs    don't change it
     Null    /dev/null on Unix, NUL on Windows
     File    open a file for reading or writing

   There is also the value Stdout, which is only meaningful for
   redirection of errors, and is performed AFTER stdout is
   redirected so that output and errors mix together. StdIn and
   StdErr could be added as well if they are useful.

   NOTE: Lots of care must be taken when redirecting stdin, stdout
   and stderr to one of EACH OTHER, since the ORDER in which they
   are changed have a significant effect on the result.
-}

type Redirects = (Redirect, Redirect, Redirect)
data Redirect = AsIs | Null | File FilePath
              | Stdout | Stderr
                deriving Show

{-
  ExecException is thrown by exec if any system call fails,
  for example because the executable we're trying to run
  doesn't exist.
-}
--                   ExecException cmd    args     redirecs  errorDesc
data ExecException = ExecException String [String] Redirects String
                     deriving (Typeable,Show)


_dev_null :: FilePath
#ifdef WIN32
_dev_null = "NUL"
#else
_dev_null = "/dev/null"
#endif

exec_interactive :: String -> String -> IO ExitCode

#ifndef WIN32
{-
This should handle arbitrary commands interpreted by the shell on Unix since
that's what people expect. But we don't want to allow the shell to interpret
the argument in any way, so we set an environment variable and call
cmd "$DARCS_ARGUMENT"
-}
exec_interactive cmd arg = withoutProgress $ do
  let var = "DARCS_ARGUMENT"
  stdin `seq` return ()
  withoutNonBlock $ bracket
    (do oldval <- getEnv var
        setEnv var arg True
        return oldval)
    (\oldval ->
       do case oldval of
            Nothing -> unsetEnv var
            Just val -> setEnv var val True)
    (\_ -> withExit127 $ system $ cmd++" \"$"++var++"\"")

#else

exec_interactive cmd arg = withoutProgress $ do system $ cmd ++ " " ++ arg
#endif

withoutNonBlock :: IO a -> IO a

#ifndef WIN32
{-
Do IO without NonBlockingRead on stdInput.

This is needed when running unsuspecting external commands with interactive
mode - if read from terminal is non-blocking also write to terminal is
non-blocking.
-}
withoutNonBlock x =
    do nb <- queryFdOption stdInput NonBlockingRead
       if nb
          then bracket
                   (do setFdOption stdInput NonBlockingRead False)
                   (\_ -> setFdOption stdInput NonBlockingRead True)
                   (\_ -> x)
          else do x
#else
withoutNonBlock x = do x
#endif

{-
Ensure that we exit 127 if the thing we are trying to run does not exist
(Only needed under Windows)
-}
withExit127 :: IO ExitCode -> IO ExitCode
#ifdef WIN32
withExit127 a = catchJust notFoundError a (const $ return $ ExitFailure 127)

notFoundError :: Exception -> Maybe ()
notFoundError (IOException e) | "runProcess: does not exist" `isInfixOf` show e = Just ()
notFoundError _ = Nothing
#else
withExit127 = id
#endif
