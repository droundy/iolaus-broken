-- Copyright (C) 2008 Eric Kow
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

-- | This modules provides rudimentary natural language generation
-- (NLG) utilities.  That is, generating natural language from a
-- machine representation.  Initially, only English is supported at
-- all.  Representations are implemented for:
--
--  * countable nouns (plurality); and
--  * lists of clauses (foo, bar and/or baz).
module Arcs.English where

import Data.List (isSuffixOf, intersperse)

-- | > englishNum 0 (Noun "watch") "" == "watches"
--   > englishNum 1 (Noun "watch") "" == "watch"
--   > englishNum 2 (Noun "watch") "" == "watches"
englishNum :: Countable n => Int -> n -> ShowS
englishNum x = if x == 1 then singular else plural

-- | Things that have a plural and singular spelling
class Countable a where
  plural :: a -> ShowS
  singular :: a -> ShowS

-- | This only distinguishes between nouns with a final -ch,
--   and nouns which do not.
--   More irregular nouns will just need to have their own type
--
--   > plural (Noun "batch") "" == "batches"
--   > plural (Noun "bat")   "" == "bats"
--   > plural (Noun "mouse") "" == "mouses" -- :-(
newtype Noun = Noun String

instance Countable Noun where
  -- more irregular nouns will just need to have their own type
  plural (Noun s) | "ch" `isSuffixOf` s = showString s .  showString "es"
  plural (Noun s) = showString s . showChar 's'
  singular (Noun s) =  showString s

-- | > singular This (Noun "batch") "" == "this batch"
--   > plural   This (Noun "batch") "" == "these batches"
data This = This Noun

instance Countable This where
  plural (This s)   = showString "these "  . plural s
  singular (This s) = showString "this "   . singular s

-- | Given a list of things, combine them thusly:
--
--   > orClauses ["foo", "bar", "baz"] == "foo, bar or baz"
andClauses, orClauses :: [String] -> String
andClauses = intersperseLast ", " " and "
orClauses  = intersperseLast ", " " or "

-- | As 'intersperse', with a different separator for the last
-- | interspersal.
intersperseLast :: String -> String -> [String] -> String
intersperseLast _ _ [] = ""
intersperseLast _ _ [clause] = clause
intersperseLast sep sepLast clauses =
    concat (intersperse sep $ init clauses) ++ sepLast ++ last clauses
