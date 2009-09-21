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
{-# LANGUAGE CPP #-}
-- , DeriveDataTypeable #-}

module Arcs.SignalHandler ( withSignalsHandled, withSignalsBlocked,
                             catchInterrupt, catchNonSignal,
                             tryNonSignal, stdout_is_a_pipe ) where

import System.IO.Error ( isUserError, ioeGetErrorString, ioeGetFileName )
import Control.Exception ( dynExceptions, ioErrors, catchJust, Exception ( IOException ) )
import System.Exit ( exitWith, ExitCode ( ExitFailure ) )
import Control.Concurrent ( ThreadId, myThreadId )
import Control.Exception ( catchDyn, throwDyn, throwDynTo, block )
import System.Posix.Files ( getFdStatus, isNamedPipe )
import System.Posix.IO ( stdOutput )
import Data.Dynamic ( Typeable, fromDynamic )
import System.IO ( hPutStrLn, stderr )
import Control.Monad ( when )

import Arcs.Workaround ( installHandler, raiseSignal, Handler(..), Signal,
                    sigINT, sigHUP, sigABRT, sigALRM, sigTERM, sigPIPE )
#ifdef WIN32
import CtrlC ( withCtrlCHandler )
#endif

stdout_is_a_pipe :: IO Bool
stdout_is_a_pipe
 = catchJust ioErrors
        (do stat <- getFdStatus stdOutput
            return (isNamedPipe stat))
        (\_ -> return False)

withSignalsHandled :: IO a -> IO a
newtype SignalException = SignalException Signal deriving (Typeable)

withSignalsHandled job = do
    thid <- myThreadId
    mapM_ (ih thid) [sigINT, sigHUP, sigABRT, sigTERM, sigPIPE]
    catchJust just_usererrors (job' thid `catchSignal` defaults)
              die_with_string
    where defaults s | s == sigINT = ew s "Interrupted!"
                     | s == sigHUP = ew s "HUP"
                     | s == sigABRT = ew s "ABRT"
                     | s == sigTERM = ew s "TERM"
                     | s == sigPIPE = exitWith $ ExitFailure $ 1
                     | otherwise = ew s "Unhandled signal!"
          ew sig s = do hPutStrLn stderr $ ("withSignalsHandled: " ++ s)
                        resethandler sig
                        raiseSignal sig -- ensure that our caller knows how we died
                        exitWith $ ExitFailure $ 1
          die_with_string e | take 6 e == "STDOUT" =
                do is_pipe <- stdout_is_a_pipe
                   when (not is_pipe) $
                        hPutStrLn stderr $ "\narcs failed:  "++drop 6 e
                   exitWith $ ExitFailure $ 2
          die_with_string e = do hPutStrLn stderr $ "\narcs failed:  "++e
                                 exitWith $ ExitFailure $ 2
#ifdef WIN32
          job' thid =
             withCtrlCHandler (throwDynTo thid $ SignalException sigINT) job
#else
          job' _ = job
#endif

resethandler :: Signal -> IO ()
resethandler s = do installHandler s Default Nothing
                    return ()

ih :: ThreadId -> Signal -> IO ()
ih thid s =
  do installHandler s (Catch $ throwDynTo thid $ SignalException s) Nothing
     return ()

catchSignal :: IO a -> (Signal -> IO a) -> IO a
catchSignal job handler =
    job `Control.Exception.catchDyn` (\(SignalException sig) -> handler sig)

-- catchNonSignal is a drop-in replacement for Control.Exception.catch, which allows
-- us to catch anything but a signal.  Useful for situations where we want
-- don't want to inhibit ctrl-C.

catchNonSignal :: IO a -> (Control.Exception.Exception -> IO a) -> IO a
catchNonSignal = Control.Exception.catchJust notSig
    where notSig x = case dynExceptions x of
                     Nothing -> Just x
                     Just d -> case fromDynamic d :: Maybe SignalException of
                               Just _ -> Nothing
                               Nothing -> Just x

catchInterrupt :: IO a -> IO a -> IO a
catchInterrupt job handler =
    job `catchSignal` h
        where h s | s == sigINT = handler
                  | otherwise   = throwDyn (SignalException s)

tryNonSignal :: IO a -> IO (Either Control.Exception.Exception a)
tryNonSignal j = (Right `fmap` j) `catchNonSignal` \e -> return (Left e)

just_usererrors :: Control.Exception.Exception -> Maybe String
just_usererrors (IOException e) | isUserError e = Just $ ioeGetErrorString e
just_usererrors (IOException e) | ioeGetFileName e == Just "<stdout>"
                                      = Just $ "STDOUT"++ioeGetErrorString e
just_usererrors _ = Nothing

withSignalsBlocked :: IO () -> IO ()
withSignalsBlocked job = (block job) `catchSignal` couldnt_do
    where couldnt_do s | s == sigINT = oops "interrupt"
                       | s ==  sigHUP = oops "HUP"
                       | s ==  sigABRT = oops "ABRT"
                       | s ==  sigALRM = oops "ALRM"
                       | s ==  sigTERM = oops "TERM"
                       | s ==  sigPIPE = return ()
                       | otherwise = oops "unknown signal"
          oops s = hPutStrLn stderr $ "Couldn't handle " ++ s ++
                   " since arcs was in a sensitive job."
