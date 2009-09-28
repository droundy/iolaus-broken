
-- Reporting bugs in darcs.  See also impossible.h.
module Grit.Bug ( _bug, _bugDoc, _impossible, _assertJust ) where

--import Autoconf( darcs_version )
import Grit.Printer ( Doc, errorDoc, text, ($$) )

type BugStuff = (String, Int, String, String)

{-# NOINLINE _bug #-}
_bug :: BugStuff -> String -> a
_bug bs s = _bugDoc bs (text s)

{-# NOINLINE _bugDoc #-}
_bugDoc :: BugStuff -> Doc -> a
_bugDoc bs s = errorDoc $ text ("bug at "++_bugLoc bs) $$ s

{-# NOINLINE _bugLoc #-}
_bugLoc :: BugStuff -> String
_bugLoc (file, line, date, time) = file++":"++show line++" compiled "++time++" "++date

{-# NOINLINE _impossible #-}
_impossible :: BugStuff -> a
_impossible bs = _bug bs $ "Impossible case at "++_bugLoc bs

{-# NOINLINE _assertJust #-}
_assertJust :: (b -> String) -> (b -> Maybe a) -> BugStuff -> b -> a
_assertJust msg f bs mx =
  case f mx of Nothing -> _bug bs $ msg mx++_bugLoc bs
               Just x  -> x
