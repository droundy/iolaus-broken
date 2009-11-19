-- Copyright (C) 2002,2003,2005,2009 David Roundy
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
{-# LANGUAGE CPP, PatternGuards #-}

module Iolaus.RunCommand ( run_the_command ) where

import Control.Monad ( when )
import Data.Maybe ( catMaybes )
import System.Console.GetOpt( ArgOrder( Permute, RequireOrder ),
                              OptDescr( Option ),
                              getOpt )
import System.Posix.Env ( setEnv )

import Iolaus.Arguments ( Flag(..), help, flagToString, disable,
                          option_from_iolausoption, pull_apart_option,
                          list_options, config_defaults )
import Iolaus.ArgumentDefaults ( add_default_flags )
import Iolaus.Command ( CommandArgs( CommandOnly, SuperCommandOnly,
                                     SuperCommandSub ),
                        Command, command_name, command_command,
                        command_basic_options, command_advanced_options,
                        command_prereq, command_extra_arg_help,
                        command_extra_args, command_argdefaults,
                        command_get_arg_possibilities,
                        command_options, command_alloptions,
                        disambiguate_commands, get_command_help,
                        get_command_mini_help, get_subcommands,
                        extract_commands, super_name, subusage )
import Iolaus.Help ( command_control_list )
import Iolaus.Global ( setDebugMode, setTimingsMode, setVerboseMode )
import Iolaus.Global ( debugMessage )
import Iolaus.RepoPath ( getCurrentDirectory )

import Git.Helpers ( configDefaults )

#include "impossible.h"

run_the_command :: String -> [String] -> IO ()
run_the_command cmd args@(h:_)
    | "--list-option":_ <- reverse args =
     do cwd <- getCurrentDirectory
        case disambiguate_commands command_control_list cmd args of
          Left _ ->
              case disambiguate_commands command_control_list cmd [h] of
                Left _ -> putStr $ unlines ["hello world","problem with fooo"]
                Right (CommandOnly c, _) ->
                    do let (opts1, opts2) = command_options cwd c
                       file_args <- command_get_arg_possibilities c
                       putStrLn $ get_options_options (opts1++opts2)
                                    ++ unlines file_args
                Right (SuperCommandOnly c, _) ->
                    do putStrLn "--help"
                       mapM_ (putStrLn . command_name)
                                 (extract_commands $ get_subcommands c)
                Right (SuperCommandSub _ _, _) -> impossible
          Right (CommandOnly c, _) ->
              do let (opts1, opts2) = command_options cwd c
                 putStrLn "--config-default"
                 file_args <- command_get_arg_possibilities c
                 putStrLn $ get_options_options (opts1++opts2)
                              ++ unlines file_args
          Right (SuperCommandOnly c, _) ->
              do putStrLn "--help"
                 mapM_ (putStrLn . command_name)
                       (extract_commands $ get_subcommands c)
          Right (SuperCommandSub _ s, _)
              | [command_name s] /= take 1 args -> putStrLn $ command_name s
          Right (SuperCommandSub _ s, _) ->
              do let (opts1, opts2) = command_options cwd s
                 putStrLn "--config-default"
                 file_args <- command_get_arg_possibilities s
                 putStrLn $ get_options_options (opts1++opts2)
                              ++ unlines file_args
run_the_command cmd args =
  either fail rtc $ disambiguate_commands command_control_list cmd args
 where
  rtc (CommandOnly c, as)       = run_command Nothing c  as
  rtc (SuperCommandOnly c,  as) = run_raw_supercommand c as
  rtc (SuperCommandSub c s, as) = run_command (Just c) s as

-- This is the actual heavy lifter code, which is responsible for parsing the
-- arguments and then running the command itself.

run_command :: Maybe Command -> Command -> [String] -> IO ()

run_command _ _ args -- Check for "dangerous" typoes...
    | "-all" `elem` args = -- -all indicates --all --look-for-adds!
        fail $ "Are you sure you didn't mean -" ++ "-all rather than -all?"
run_command msuper cmd args = do
   cwd <- getCurrentDirectory
   let options = opts1 ++ opts2
                 ++ concatMap (option_from_iolausoption cwd) config_defaults
       (opts1, opts2) = command_options cwd cmd
   case getOpt Permute
             (option_from_iolausoption cwd list_options++options) args of
    (opts,extra,[])
      | Help `elem` opts -> putStrLn $ get_command_help msuper cmd
      | ConfigDefault `elem` opts ->
          configDefaults (command_name `fmap` msuper) (command_name cmd)
                  (map (\o -> (`pull_apart_option` o)) iopts) opts
      | ListOptions `elem` opts  -> do
           command_prereq cmd opts
           file_args <- command_get_arg_possibilities cmd
           putStrLn $ get_options_options options ++ unlines file_args
      | otherwise -> consider_running msuper cmd (addVerboseIfDebug opts) extra
    (_,_,ermsgs) -> fail $ unlines ermsgs
    where addVerboseIfDebug opts | DebugVerbose `elem` opts = Debug:Verbose:opts
                                 | otherwise = opts
          iopts = disable : command_basic_options cmd ++
                  command_advanced_options cmd

consider_running :: Maybe Command -> Command
                 -> [Flag] -> [String] -> IO ()
consider_running msuper cmd opts old_extra = do
 cwd <- getCurrentDirectory
 location <- command_prereq cmd opts
 case location of
   Left complaint -> fail $ "Unable to " ++
                     show ("iolaus "++super_name msuper++command_name cmd) ++
                     " here.\n\n" ++ complaint
   Right () -> do
    specops <- add_command_defaults cmd opts
    extra <- (command_argdefaults cmd) specops cwd old_extra
    when (Disable `elem` specops) $
      fail $ "Command "++command_name cmd++" disabled with --disable option!"
    if command_extra_args cmd < 0
      then runWithHooks specops extra
      else if length extra > command_extra_args cmd
           then fail $ "Bad argument: `"++unwords extra++"'\n"++
                       get_command_mini_help msuper cmd
           else if length extra < command_extra_args cmd
                then fail $ "Missing argument:  " ++
                            nth_arg (length extra + 1) ++
                            "\n" ++ get_command_mini_help msuper cmd
                else runWithHooks specops extra
       where nth_arg n = nth_of n (command_extra_arg_help cmd)
             nth_of 1 (h:_) = h
             nth_of n (_:hs) = nth_of (n-1) hs
             nth_of _ [] = "UNDOCUMENTED"
             runWithHooks os ex = do
               here <- getCurrentDirectory
               -- set any global variables
               when (Timings `elem` os) setTimingsMode
               when (Debug `elem` os) setDebugMode
               when (Verbose `elem` os) setVerboseMode
               case [a | Author a <- os] of
                 a:_ -> do setEnv "GIT_AUTHOR_NAME" (takeWhile (/='<') a) True
                           setEnv "GIT_AUTHOR_EMAIL"
                              (takeWhile (/='>') $ drop 1 $ dropWhile (/='<') a)
                              True
                 [] -> return ()
               let allopts = case command_alloptions cmd of (x,y) -> x++y
               debugMessage $ unwords ("Running":"iolaus":
                                       maybe [] ((:[]) . command_name) msuper++
                                       command_name cmd:
                                       catMaybes (map (flagToString allopts) os)++ex)
               let fixFlag = FixFilePath here cwd
               (command_command cmd) (fixFlag : os) ex

add_command_defaults :: Command -> [Flag] -> IO [Flag]
add_command_defaults cmd already = do
  let (opts1, opts2) = command_alloptions cmd
  add_default_flags (command_name cmd) (opts1 ++ opts2) already

get_options_options :: [OptDescr Flag] -> String
get_options_options [] = ""
get_options_options (o:os) =
    get_long_option o ++"\n"++ get_options_options os

get_long_option :: OptDescr Flag -> String
get_long_option (Option _ [] _ _) = ""
get_long_option (Option a (o:os) b c) = "--"++o++
                 get_long_option (Option a os b c)

run_raw_supercommand :: Command -> [String] -> IO ()
run_raw_supercommand super [] =
    fail $ "Command '"++ command_name super ++"' requires subcommand!\n\n"
             ++ subusage super
run_raw_supercommand super args = do
  cwd <- getCurrentDirectory
  case getOpt RequireOrder
             (option_from_iolausoption cwd help++
              option_from_iolausoption cwd list_options) args of
    (opts,_,[])
      | Help `elem` opts ->
            putStrLn $ get_command_help Nothing super
      | ListOptions `elem` opts -> do
            putStrLn "--help"
            mapM_ (putStrLn . command_name) (extract_commands $ get_subcommands super)
      | otherwise ->
            if Disable `elem` opts
            then fail $ "Command " ++ (command_name super) ++
                      " disabled with --disable option!"
            else fail $ "Invalid subcommand!\n\n" ++ subusage super
    (_,_,ermsgs) -> fail $ unlines ermsgs
