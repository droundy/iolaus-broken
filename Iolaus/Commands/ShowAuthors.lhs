%  Copyright (C) 2004-2009 David Roundy, Eric Kow, Simon Michael
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

\subsubsection{iolaus show authors}
\begin{code}
module Iolaus.Commands.ShowAuthors ( show_authors ) where

import Data.List ( sort, group, isInfixOf, isPrefixOf )
import Data.Char ( toLower, isSpace )
import Text.Regex ( Regex, mkRegexWithOpts, matchRegex )

import Iolaus.Arguments ( Flag(..), working_repo_dir )
import Iolaus.Command ( Command(..), nodefaults )
import Git.LocateRepo ( amInRepository )
import Iolaus.Lock ( readBinFile )

import Git.Plumbing ( RevListOption(Authors) )
import Git.Helpers ( revListHeads )
\end{code}

\options{show authors}

\haskell{show_authors_help}

\begin{code}
show_authors_description :: String
show_authors_description = "Show all authors in the repository."
\end{code}

\begin{code}
show_authors_help :: String
show_authors_help =
 "The authors command writes a list of all authors in the repository to\n" ++
 "standard output."
\end{code}

\begin{code}
show_authors :: Command
show_authors = Command {
  command_name = "authors",
  command_help = show_authors_help,
  command_description = show_authors_description,
  command_extra_args = 0,
  command_extra_arg_help = [],
  command_command = authors_cmd,
  command_prereq = amInRepository,
  command_get_arg_possibilities = return [],
  command_argdefaults = nodefaults,
  command_advanced_options = [],
  command_basic_options = [working_repo_dir] }

authors_cmd :: [Flag] -> [String] -> IO ()
authors_cmd opts _ =
    do authors <- (filter ((/="commit") . take 6) . lines)
                  `fmap` revListHeads [Authors]
       spellings <- compiled_author_spellings
       putStr $ unlines $ reverse $ map shownames $ sort $
              map (\s -> (length s,head s)) $ group $ sort $ concat $ map (\(n,a) ->  replicate n a) $
              map (\s -> (length s,canonize_author spellings $ head s)) $ group $ sort authors
  where shownames (n, a) = if Verbose `elem` opts 
                           then show n ++ "\t" ++ a
                           else a

canonize_author :: [(String,[Regex])] -> String -> String
canonize_author [] a = a
canonize_author spellings a = safehead a $ canonicalsfor a
    where
      safehead x xs = if null xs then x else head xs
      canonicalsfor s = map fst $ filter (ismatch s) spellings
      ismatch s (canonical,regexps) =
          (not (null email) && (s `contains` email)) || (any (s `contains_regex`) regexps)
          where email = takeWhile (/= '>') $ drop 1 $ dropWhile (/= '<') canonical

contains :: String -> String -> Bool
a `contains` b = lower b `isInfixOf` (lower a) where lower = map toLower

contains_regex :: String -> Regex -> Bool
a `contains_regex` r = case matchRegex r a of
                         Just _ -> True
                         _ -> False

compiled_author_spellings :: IO [(String,[Regex])]
compiled_author_spellings = do
  ss <- author_spellings_from_file
  return $ map compile $ ss
    where
      compile [] = error "each author spelling should contain at least the canonical form"
      compile (canonical:pats) = (canonical, map mkregex pats)
      mkregex pat = mkRegexWithOpts pat True False

-- Canonical author spellings can be defined in this file, to clean up
-- the output of show authors. Each line contains one or more
-- comma-separated fields: the canonical name and email address in
-- angle brackets, optionally followed by additional regular
-- expression patterns. Blank lines and lines beginning with -- are
-- ignored. An author string which contains the canonical email
-- address or any of the patterns will be replaced by the full
-- canonical form.  All matching is case-insensitive. To match the
-- whole author string use ^ and $.
authorspellingsfile :: FilePath
authorspellingsfile = ".authorspellings"

author_spellings_from_file :: IO [[String]]
author_spellings_from_file = do
  s <- readBinFile authorspellingsfile `catch` (\_ -> return "")
  let noncomments = filter (not . ("--" `isPrefixOf`)) $
                    filter (not . null) $ map strip $ lines s
  return $ map (map strip . split_on ',') noncomments

split_on :: Eq a => a -> [a] -> [[a]]
split_on e l =
    case dropWhile (e==) l of
      [] -> []
      l' -> first : split_on e rest
        where
          (first,rest) = break (e==) l'

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

\end{code}
