-- Copyright (C) 2002-2003,2009 David Roundy
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

{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- , ScopedTypeVariables, TypeOperators, PatternGuards #-}

#include "gadts.h"

module Iolaus.SelectCommits ( select_commits, select_last_commits,
                              select_commit, isMerge ) where

import System.IO
import Data.List ( intersperse, partition, isInfixOf )
import Data.Char ( toUpper )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Control.Monad ( filterM )

import Iolaus.Flags ( Flag( All, SeveralPatch, Verbose, Summary, DryRun,
                            ShowMerges, Graph, DateRelative ) )
import Iolaus.Utils ( promptCharFancy )
import Iolaus.Sealed ( Sealed( Sealed ), mapSealM, unseal )
import Iolaus.Printer ( putDocLn )
import Iolaus.Graph ( putGraph )

import Git.Dag ( iao )
import Git.Plumbing ( Hash, Commit, catCommit, myMessage, myParents )
import Git.Helpers ( showCommit )

data WhichChanges = Last | First | One
                    deriving (Eq, Show)

isMerge :: Sealed (Hash Commit) -> IO Bool
isMerge (Sealed x) = do ce <- catCommit x
                        return (take 5 (myMessage ce) == "Merge" &&
                                length (myParents ce) > 1) 

match :: [Flag] -> Sealed (Hash Commit) -> IO Bool
match opts x =
    do ismerge <- isMerge x
       if ShowMerges `notElem` opts && ismerge
         then return False
         else matchPat opts x

matchPat :: [Flag] -> Sealed (Hash Commit) -> IO Bool
matchPat (SeveralPatch p:_) x =
    do Sealed ce <- mapSealM catCommit x
       return (p `isInfixOf` myMessage ce)
matchPat (_:fs) x = matchPat fs x
matchPat [] _ = return True

select_commit :: String -> [Flag] -> [Sealed (Hash Commit)]
               -> IO (Sealed (Hash Commit))
select_commit jn _ [] = do putStrLn ("There is no commit to "++jn++"!")
                           exitWith ExitSuccess
select_commit jn opts cs0 =
    do -- in select_commit, we want a commit that has no descendents.
       -- select_commit is used in commands like amend-record and
       -- unrecord, and it always should return a commit that could be
       -- removed without touching any other commits.
       let islast x = not $ any (x `iao`) cs0
       cs <- filterM (match opts) $ filter islast cs0
       if DryRun `elem` opts
          then do if null cs
                     then putStrLn ("No commit to "++jn++".")
                     else putStrLn ("Would "++jn++" the following commit:")
                  putGraph (Graph:opts++[DateRelative]) (`elem` cs) $ take 1 cs
                  exitWith ExitSuccess
          else do xs <- text_select One [] jn opts cs []
                  case xs of
                    x:_ -> return x
                    [] -> fail "No commit selected?!"

select_commits :: String -> [Flag] -> [Sealed (Hash Commit)]
               -> IO [Sealed (Hash Commit)]
select_commits jn opts cs0 =
    do cs <- filterM (match opts) cs0
       if DryRun `elem` opts
          then do if null cs
                     then putStrLn ("No commits to "++jn++".")
                     else putStrLn ("Would "++jn++" the following commits:")
                  putGraph (Graph:opts++[DateRelative]) (`elem` cs) cs
                  exitWith ExitSuccess
          else text_select First [] jn opts cs []

select_last_commits :: String -> [Flag] -> [Sealed (Hash Commit)]
                    -> IO [Sealed (Hash Commit)]
select_last_commits jn opts cs0 =
    do cs <- filterM (match opts) cs0
       if DryRun `elem` opts
          then do if null cs
                     then putStrLn ("No commits to "++jn++".")
                     else putStrLn ("Would "++jn++" the following commits:")
                  putGraph (Graph:opts++[DateRelative]) (`elem` cs) cs
                  exitWith ExitSuccess
          else text_select Last [] jn opts cs []

text_select :: WhichChanges -> [Sealed (Hash Commit)]
            -> String -> [Flag] -> [Sealed (Hash Commit)] -> [Flag]
            -> IO [Sealed (Hash Commit)]
text_select _ sofar _ _ [] _ = return sofar
text_select w sofar _ opts cs _
    | All `elem` opts && w == One = return (take 1 $ sofar++cs)
    | All `elem` opts = return (sofar++cs)
text_select w sofar jn opts (c:cs) showopts =
    do showCommit (showopts++opts++[DateRelative]) `unseal` c >>= putDocLn
       doKey prompt options
    where
        options_basic =
           [ KeyPress 'y' (jn++" this patch") $
             case w of
               First ->
                   case partition (`iao` c) cs of
                     (ans,oths) -> text_select w (c:ans++sofar) jn opts oths []
               Last ->
                   case partition (c `iao`) cs of
                     (ans,oths) -> text_select w (c:ans++sofar) jn opts oths []
               One -> return [c]
           , KeyPress 'n' ("don't "++jn++" it") $
             case w of
               First -> text_select w sofar jn opts
                        (filter (not . (c `iao`)) cs) []
               Last -> text_select w sofar jn opts
                       (filter (not . (`iao` c)) cs) []
               One -> text_select w sofar jn opts cs []
           , KeyPress 'w' ("wait and decide later") $
             text_select w sofar jn opts (cs++[c]) []]
        options_view =
           [ KeyPress 'v' ("view this patch in full")
             $ text_select w sofar jn opts (c:cs) [Verbose]
           , KeyPress 'p' ("view this patch in full with pager")
           $ text_select w sofar jn opts (c:cs) [Verbose] ]
        options_summary =
           [ KeyPress 'x' ("view a summary of this patch")
           $ text_select w sofar jn opts (c:cs) [Summary] ]
        options_help =
           [ KeyPress '?' ("show this help")
           $ do putStrLn $ helpFor jn options
                text_select w sofar jn opts (c:cs) [] ]
        options_quit :: [KeyPress [Sealed (Hash Commit)]]
        options_quit =
           [ KeyPress 'd'
             (jn++" selected patches, skipping all the remaining patches")
             (return sofar)
           , KeyPress 'a' (jn++" all the remaining patches")
             (return (sofar++c:cs))
           , KeyPress 'q' ("cancel "++jn)
             $ do putStrLn $ jn_cap++" cancelled."
                  exitWith $ ExitSuccess ]
        options :: [[KeyPress [Sealed (Hash Commit)]]]
        options = [options_basic]
                  ++ [options_view ++ options_summary]
                  ++ [options_quit, options_help]
        prompt = "Shall I "++jn++" this patch? "
               ++ case length cs of
                    0 -> ""
                    1 -> "( one more to go ) "
                    n -> "( " ++ show n ++ " more to go) "
        jn_cap = (toUpper $ head jn) : tail jn

doKey :: String -> [[KeyPress a]] -> IO a
doKey prompt keys =
    do c <- promptCharFancy prompt (keysFor keys) Nothing "?h"
       case filter ((==c) . kp) $ concat keys of
         kk:_ -> kpJob kk
         [] -> doKey prompt keys

data KeyPress a = KeyPress { kp     :: Char,
                             kpHelp :: String,
                             kpJob :: IO a }

helpFor :: String -> [[KeyPress a]] -> String
helpFor jobname options =
  unlines $ [ "How to use "++jobname++":" ]
            ++ (concat $ intersperse [""] $ map (map help) options)
            ++ [ ""
               , "?: show this help"
               , ""
               , "<Space>: accept the current default (which is capitalized)"
               ]
  where help i = kp i:(": "++kpHelp i)

keysFor :: [[KeyPress a]] -> [Char]
keysFor = filter (`notElem` "h?") . concatMap (map kp)
