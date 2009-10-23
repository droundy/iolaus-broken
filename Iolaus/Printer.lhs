
A Document is at heart ShowS from the prelude
\htmladdnormallink{http://www.haskell.org/onlinereport/standard-prelude.html#\$tShowS}

Essentially, if you give a Doc a string it'll print out whatever it
wants followed by that string. So \verb!(text "foo")! makes the Doc that
prints \verb!"foo"! followed by its argument. The combinator names are taken
from Text.PrettyPrint.HughesPJ, although the behaviour of the two libraries is
slightly different.

The advantage of Printer over simple string appending/concatenating is
that the appends end up associating to the right, e.g.:

\begin{verbatim}
  (text "foo" <> text "bar") <> (text "baz" <> text "quux") ""
= \s -> (text "foo" <> text "bar") ((text "baz" <> text "quux") s) ""
= (text "foo" <> text "bar") ((text "baz" <> text "quux") "")
= (\s -> (text "foo") (text "bar" s)) ((text "baz" <> text "quux") "")
= text "foo" (text "bar" ((text "baz" <> text "quux") ""))
= (\s -> "foo" ++ s) (text "bar" ((text "baz" <> text "quux") ""))
= "foo" ++ (text "bar" ((text "baz" <> text "quux") ""))
= "foo" ++ ("bar" ++ ((text "baz" <> text "quux") ""))
= "foo" ++ ("bar" ++ ((\s -> text "baz" (text "quux" s)) ""))
= "foo" ++ ("bar" ++ (text "baz" (text "quux" "")))
= "foo" ++ ("bar" ++ ("baz" ++ (text "quux" "")))
= "foo" ++ ("bar" ++ ("baz" ++ ("quux" ++ "")))
\end{verbatim}

The Empty alternative comes in because you want
\begin{verbatim}
    text "a" $$ vcat xs $$ text "b"
\end{verbatim}
(\verb!$$! means ``above'', vcat is the list version of \verb!$$!) to be
\verb!"a\nb"! when \verb!xs!  is \verb![]!, but without the concept of an
Empty Document each \verb!$$! would add a \verb!'\n'! and you'd end up with
\verb!"a\n\nb"!. Note that \verb!Empty /= text ""! (the latter would cause two
\verb!'\n'!s).

This code was made generic in the element type by Juliusz Chroboczek.
\begin{code}
module Iolaus.Printer
    (Doc, Color(..),
     hPutDoc, hPutDocLn, putDoc, putDocLn, renderString, renderPSs,
     prefix, colorPS, text, printable, wrap_text, blueText, redText, greenText,
     unsafeText, packedString, unsafePackedString, userchunkPS,
     empty, (<>), (<?>), (<+>), ($$), vcat, vsep, hcat,
     minus, newline, plus, space,
     traceDoc, assertDoc, errorDoc ) where

import Debug.Trace ( trace )
import Data.Char ( isAscii, isPrint, isSpace, isControl, ord, chr, intToDigit )
import System.IO.Unsafe ( unsafePerformIO )
import System.IO ( hIsTerminalDevice )
import System.Environment ( getEnv )
import Data.Bits ( bit, xor )

import Data.List (intersperse)
import System.IO (Handle, stdout, hPutStr)
import qualified Data.ByteString as B (ByteString, hPut, null,
                                       init, span, spanEnd, elem)
import qualified Data.ByteString.Char8 as BC
    (unpack, pack, singleton, spanEnd, any, last)

-- | A 'Printable' is either a String, a packed string, or a chunk of
-- text with both representations.
data Printable = S !String
               | PS !B.ByteString
               | Both !String !B.ByteString

-- | 'space_p' is the 'Printable' representation of a space.
space_p :: Printable
space_p   = PS (BC.singleton ' ')

-- | 'newline_p' is the 'Printable' representation of a newline.
newline_p :: Printable
newline_p = PS (BC.singleton '\n')

-- | Minimal 'Doc's representing the common characters 'space', 'newline'
-- 'minus', 'plus', and 'backslash'.
space, newline, plus, minus, backslash :: Doc
space     = unsafeBoth " "  (BC.singleton ' ')
newline   = unsafeChar '\n'
minus     = unsafeBoth "-"  (BC.singleton '-')
plus      = unsafeBoth "+"  (BC.singleton '+')
backslash = unsafeBoth "\\" (BC.singleton '\\')

errorDoc :: Doc -> a
errorDoc = error . renderStringWith simplePrinters'


putDoc :: Doc -> IO ()
putDoc = hPutDoc stdout

putDocLn :: Doc -> IO ()
putDocLn = hPutDocLn stdout

-- | 'hputDocWith' puts a Doc on the given handle using the given printer.
hPutDocWith :: Printers -> Handle -> Doc -> IO ()
-- | 'hputDocLnWith' puts a doc, followed by a newline on the given
-- handle using the given printer.
hPutDocLnWith :: Printers -> Handle -> Doc -> IO ()

hPutDocWith prs h d = hPrintPrintables h (renderWith (prs h) d)
hPutDocLnWith prs h d = hPutDocWith prs h (d <?> newline)

-- |'hputDoc' puts a Doc on the given handle using 'simplePrinters'
hPutDoc :: Handle -> Doc -> IO ()
-- 'hputDocLn' puts a doc, followed by a newline on the given handle using
-- 'simplePrinters'.
hPutDocLn :: Handle -> Doc -> IO ()
hPutDoc = hPutDocWith fancyPrinters
hPutDocLn = hPutDocLnWith fancyPrinters

-- | @'hPrintPrintables' h@ prints a list of 'Printable's to the handle h
hPrintPrintables :: Handle -> [Printable] -> IO ()
hPrintPrintables h = mapM_ (hPrintPrintable h)

-- | @hPrintPrintable h@ prints a 'Printable' to the handle h.
hPrintPrintable :: Handle -> Printable -> IO ()
hPrintPrintable h (S ps) = hPutStr h ps
hPrintPrintable h (PS ps) = B.hPut h ps
hPrintPrintable h (Both _ ps) = B.hPut h ps

-- | a 'Doc' is a bit of enriched text. 'Doc's get concatanated using
-- '<>', which is right-associative.
newtype Doc = Doc { unDoc :: St -> Document }

-- | The State associated with a doc. Contains a set of printers for each
-- hanlde, and the current prefix of the document.
data St = St { printers :: !Printers',
               current_prefix :: !([Printable] -> [Printable]) }
type Printers = Handle -> Printers'

-- | A set of printers to print different types of text to a handle.
data Printers' = Printers {colorP :: !(Color -> Printer),
                           userchunkP :: !Printer,
                           defP :: !Printer }
type Printer = Printable -> St -> Document

data Color = Blue | Red | Green | Cyan | Magenta
           | Underline Color

-- | 'Document' is a wrapper around '[Printable] -> [Printable]' which allows
-- for empty Documents. The simplest 'Documents' are built from 'String's
-- using 'text'.
data Document = Document ([Printable] -> [Printable])
              | Empty

-- | renders a 'Doc' into a 'String' with control codes for the
-- special features of the doc.
renderString :: Doc -> String
renderString = renderStringWith simplePrinters'

-- | renders a 'Doc' into a 'String' using a given set of printers.
renderStringWith :: Printers' -> Doc -> String
renderStringWith prs d = concatMap toString $ renderWith prs d
    where toString (S s) = s
          toString (PS ps) = BC.unpack ps
          toString (Both s _) = s

-- | renders a 'Doc' into a list of 'PackedStrings', one for each line.
renderPSs :: Doc -> [B.ByteString]
renderPSs = renderPSsWith simplePrinters'

-- | renders a 'Doc' into a list of 'PackedStrings', one for each
-- chunk of text that was added to the doc, using the given set of
-- printers.
renderPSsWith :: Printers' -> Doc -> [B.ByteString]
renderPSsWith prs d = map toPS $ renderWith prs d
    where toPS (S s)        = BC.pack s
          toPS (PS ps)      = ps
          toPS (Both _ ps)  = ps

-- | renders a 'Doc' into a list of 'Printables' using a set of
-- printers. Each item of the list corresponds to a string that was
-- added to the doc.
renderWith :: Printers' -> Doc -> [Printable]
renderWith ps (Doc d) = case d (init_state ps) of
                        Empty -> []
                        Document f -> f []

init_state :: Printers' -> St
init_state prs = St { printers = prs, current_prefix = id }

prefix :: String -> Doc -> Doc
prefix s (Doc d) = Doc $ \st ->
                   let p = S s
                       st' = st { current_prefix = current_prefix st . (p:) } in
                   case d st' of
                     Document d'' -> Document $ (p:) . d''
                     Empty -> Empty

-- | 'unsafeBoth' builds a Doc from a 'String' and a 'B.ByteString' representing
-- the same text, but does not check that they do.
unsafeBoth :: String -> B.ByteString -> Doc
unsafeBoth s ps = Doc $ simplePrinter (Both s ps)

-- | 'unsafeBothText' builds a 'Doc' from a 'String'. The string is
-- stored in the Doc as both a String and a 'B.ByteString'.
unsafeBothText :: String -> Doc
unsafeBothText s = Doc $ simplePrinter (Both s (BC.pack s))

-- | 'packedString' builds a 'Doc' from a 'B.ByteString' using 'printable'
packedString :: B.ByteString -> Doc
packedString = printable . PS

-- | 'unsafePackedString' builds a 'Doc' from a 'B.ByteString'
unsafePackedString :: B.ByteString -> Doc
unsafePackedString = Doc . simplePrinter . PS

-- | 'userchunkPS' creates a 'Doc' representing a user chunk from a
-- | 'B.ByteString'.
userchunkPS :: B.ByteString -> Doc
userchunkPS = userchunkPrintable . PS

-- | 'unsafeChar' creates a Doc containing just one character.
unsafeChar :: Char -> Doc
unsafeChar = unsafeText . (:"")

-- | 'text' creates a 'Doc' from a @String@, using 'printable'.
text :: String -> Doc
-- | 'unsafeText' creates a 'Doc' from a 'String', using 'simplePrinter' directly
unsafeText :: String -> Doc
-- | 'blueText' creates a 'Doc' containing blue text from a @String@
blueText, redText, greenText :: String -> Doc
text = printable . S
unsafeText = Doc . simplePrinter . S
blueText = colorText Blue
redText = colorText Red
greenText = colorText Green

-- | 'colorText' creates a 'Doc' containing colored text from a @String@
colorText :: Color -> String -> Doc
colorText c = mkColorPrintable c . S

colorPS :: Color -> B.ByteString -> Doc
colorPS c ps
    | B.null ps = empty
    | otherwise = case B.span isWhite ps of
                  (w,more) ->
                     case B.spanEnd isWhite more of
                       (body,w2) ->
                           mkColorPrintable (Underline c) (PS w) <>
                           mkColorPrintable c (PS body) <>
                           mkColorPrintable (Underline c) (PS w2)
    where isWhite = (`B.elem` (BC.pack "\n\t "))

-- | @'wrap_text' n s@ is a 'Doc' representing @s@ line-wrapped at 'n' characters
wrap_text :: Int -> String -> Doc
wrap_text n s =
    vcat $ map text $ reverse $ "": (foldl add_to_line [] $ words s)
  where add_to_line [] a = [a]
        add_to_line ("":d) a = (a:d)
        add_to_line (l:ls) new | length l + length new > n = new:l:ls
        add_to_line (l:ls) new = (l ++ " " ++ new):ls

-- | 'printable x' creates a 'Doc' from any 'Printable'.
printable, userchunkPrintable :: Printable -> Doc
printable x = Doc $ \st -> defP (printers st) x st

mkColorPrintable :: Color -> Printable -> Doc
mkColorPrintable c x = Doc $ \st -> colorP (printers st) c x st
userchunkPrintable x = Doc $ \st -> userchunkP (printers st) x st

-- | A set of default printers suitable for any handle. Does not use color.
simplePrinters' :: Printers'
simplePrinters'  = Printers { colorP = const simplePrinter,
                              userchunkP = simplePrinter,
                              defP = simplePrinter
                            }

-- | 'simplePrinter' is the simplest 'Printer': it just concatenates together
-- the pieces of the 'Doc'
simplePrinter :: Printer
simplePrinter x = unDoc $ doc (\s -> x:s)

infixr 6 <>
infixr 6 <+>
infixr 5 $$

-- | The empty 'Doc'.
empty :: Doc
empty = Doc $ const Empty
doc :: ([Printable] -> [Printable]) -> Doc
doc f = Doc $ const $ Document f

-- | '(<>)' is the concatenation operator for 'Doc's
(<>) :: Doc -> Doc -> Doc
-- | @a '<?>' b@ is @a@ if it is not empty, else @b@.
(<?>) :: Doc -> Doc -> Doc
-- | @a '<+>' b@ is @a@ followed by a space, then @b@.
(<+>) :: Doc -> Doc -> Doc
-- | @a '$$' b@ is @a@ above @b@.
($$) :: Doc -> Doc -> Doc
-- a then b
Doc a <> Doc b =
   Doc $ \st -> case a st of
                Empty -> b st
                Document af ->
                    Document (\s -> af $ case b st of
                                         Empty -> s
                                         Document bf -> bf s)

-- empty if a empty, else a then b
Doc a <?> Doc b =
    Doc $ \st -> case a st of
                 Empty -> Empty
                 Document af -> Document (\s -> af $ case b st of
                                                     Empty -> s
                                                     Document bf -> bf s)

-- a then space then b
Doc a <+> Doc b =
    Doc $ \st -> case a st of
                 Empty -> b st
                 Document af -> Document (\s -> af $ case b st of
                                                     Empty -> s
                                                     Document bf ->
                                                         space_p:bf s)

-- a above b
Doc a $$ Doc b =
   Doc $ \st -> case a st of
                Empty -> b st
                Document af ->
                    Document (\s -> af $ case b st of
                                         Empty -> s
                                         Document bf -> newline_p:pf (bf s))
                        where pf = current_prefix st

-- | 'vcat' piles vertically a list of 'Doc's.
vcat :: [Doc] -> Doc
vcat [] = empty
vcat ds = foldr1 ($$) ds

-- | 'vsep' piles vertically a list of 'Doc's leaving a blank line between each.
vsep :: [Doc] -> Doc
vsep [] = empty
vsep ds = foldr1 ($$) $ intersperse (text "") ds

-- | 'hcat' concatenates (horizontally) a list of 'Doc's
hcat :: [Doc] -> Doc
hcat [] = empty
hcat ds = foldr1 (<>) ds
\end{code}

\begin{code}
dollar, cr :: Doc
dollar = unsafeBothText "$"
cr     = unsafeBothText "\r"

traceDoc :: Doc -> a -> a
traceDoc _ = trace "oops"

assertDoc :: Maybe Doc -> a -> a
assertDoc Nothing x = x
assertDoc (Just e) _ = errorDoc e

-- policy
-- | the 'Policy' type is a record containing the variables which control
-- how 'Doc's will be rendered on some output.
data Policy = Policy { poColor :: Bool    -- ^ overall use of color
                     , poEscape :: Bool   -- ^ overall use of escaping
                     , poLineColor :: Bool -- ^ overall use of colored lines (only hunks for now)
                     , poAltColor :: Bool -- ^ alternative to color (bold, inverse)
                     , poIsprint :: Bool  -- ^ don't escape isprints
                     , po8bit  :: Bool    -- ^ don't escape 8-bit chars
                     , poNoEscX :: String   -- ^ extra chars to never escape
                     , poEscX :: String   -- ^ extra chars to always escape
                     , poTrailing :: Bool -- ^ escape trailing spaces
                     , poCR :: Bool       -- ^ ignore \r at end of lines
                     , poSpace :: Bool    -- ^ escape spaces (used with poTrailing)
                     }

{-# NOINLINE getPolicy #-}
-- | 'getPolicy' returns a suitable policy for a given handle.
-- The policy is chosen according to environment variables, and to the
-- type of terminal which the handle represents
getPolicy :: Handle -> Policy
getPolicy handle = unsafePerformIO $
 do isTerminal <- hIsTerminalDevice handle
    envDontEscapeAnything  <- getEnvBool "DARCS_DONT_ESCAPE_ANYTHING"
    envDontEscapeIsprint   <- getEnvBool "DARCS_DONT_ESCAPE_ISPRINT"
    envUseIsprint          <- getEnvBool "DARCS_USE_ISPRINT" -- for backwards-compatibility
    envDontEscape8bit      <- getEnvBool "DARCS_DONT_ESCAPE_8BIT"

    envDontEscapeExtra  <- getEnvString "DARCS_DONT_ESCAPE_EXTRA"
    envEscapeExtra      <- getEnvString "DARCS_ESCAPE_EXTRA"

    envDontEscapeTrailingSpace  <- getEnvBool "DARCS_DONT_ESCAPE_TRAILING_SPACES"
    envDontEscapeTrailingCR     <- getEnvBool "DARCS_DONT_ESCAPE_TRAILING_CR"

    envDontColor         <- getEnvBool "DARCS_DONT_COLOR"
    envAlwaysColor       <- getEnvBool "DARCS_ALWAYS_COLOR"
    envAlternativeColor  <- getEnvBool "DARCS_ALTERNATIVE_COLOR"
    envDoColorLines    <- getEnvBool "DARCS_DO_COLOR_LINES"

    let haveColor = envAlwaysColor || isTerminal
        doColor   = not envDontColor && haveColor

    return Policy { poColor    = doColor,
                    poEscape   = not envDontEscapeAnything,
                    poLineColor= doColor && envDoColorLines,
                    poIsprint  = envDontEscapeIsprint || envUseIsprint,
                    po8bit     = envDontEscape8bit,
                    poNoEscX   = envDontEscapeExtra,
                    poEscX     = envEscapeExtra,
                    poTrailing = not envDontEscapeTrailingSpace,
                    poCR       = envDontEscapeTrailingCR,
                    poAltColor = haveColor && envAlternativeColor,

                    poSpace = False
                  }
 where
  getEnvBool s = safeGetEnv s >>= return.(/= "0")
  safeGetEnv s = getEnv s `catch` \_ -> return "0"
  getEnvString s = getEnv s `catch` \_ -> return ""


-- printers

-- | @'fancyPrinters' h@ returns a set of printers suitable for outputting
-- to @h@
fancyPrinters :: Printers
fancyPrinters h = Printers { colorP     = colorPrinter (getPolicy h),
                             userchunkP  = userchunkPrinter (getPolicy h),
                             defP       = escapePrinter (getPolicy h) }

colorPrinter :: Policy -> Color -> Printer
colorPrinter po | poColor po = \c -> unDoc . color po c . Doc . escapePrinter po{poColor=False}
                | otherwise  = const $ escapePrinter po

userchunkPrinter :: Policy -> Printer
userchunkPrinter po p
 | not (poEscape po)   = simplePrinter p
 | not (poTrailing po) = escapePrinter po p
 | otherwise           = unDoc $ pr p
 where
  pr (S s)       = prString s
  pr (Both _ ps) = prPS ps
  pr (PS ps)     = prPS ps

  prPS ps = let (leadPS, trailPS) = BC.spanEnd isSpace ps
            in if B.null trailPS
                then Doc $ escapePrinter po p
                else Doc (escapePrinter po (PS leadPS))
                  <> Doc (escapePrinter po{poSpace=True} (PS trailPS))
                  <> mark_escape po dollar

  prString s = let (trail',lead') = span isSpace (reverse s)
                   lead = reverse lead'
                   trail = reverse trail'
               in if (not.null) trail
                   then Doc (escapePrinter po (S lead))
                     <> Doc (escapePrinter po{poSpace=True} (S trail))
                     <> mark_escape po dollar
                   else Doc (escapePrinter po p)

escapePrinter :: Policy -> Printer
escapePrinter po
 | (not.poEscape) po = simplePrinter
 | otherwise         = unDoc . crepr
 where
  crepr p | poCR po && isEndCR p = epr (initPR p) <> cr
          | otherwise            = epr p

  epr (S s)      = escape po s
  epr (PS ps)    = if BC.any (not.no_escape po) ps
                   then escape po (BC.unpack ps)
                   else unsafePackedString ps
  epr (Both s _) = escape po s

  isEndCR (S s)        = not (null s) && last s == '\r'
  isEndCR (PS ps)      = not (B.null ps) && BC.last ps == '\r'
  isEndCR (Both _ ps)  = not (B.null ps) && BC.last ps == '\r'

  initPR (S s)       = S $ init s
  initPR (PS ps)     = PS $ B.init ps
  initPR (Both s ps) = Both (init s) (B.init ps)


-- escape assumes the input is in ['\0'..'\255']

-- | @'escape' policy string@ escapes @string@ according to the rules
-- defined in 'policy', turning it into a 'Doc'.
escape :: Policy -> String -> Doc
escape _ "" = unsafeText ""
escape po s = hcat (map escapeChar s)
 where
  escapeChar c | no_escape po c = unsafeChar c
  escapeChar ' ' = space
  escapeChar c = (emph.unsafeText.quoteChar) c
  emph = mark_escape po

-- | @'no_escape' policy c@ tells wether @c@ will be left as-is
-- when escaping according to @policy@
no_escape :: Policy -> Char -> Bool
no_escape po c | poSpace po && isSpace c = False
no_escape po c | c `elem` poEscX po = False
no_escape po c | c `elem` poNoEscX po = True
no_escape _ '\t' = True  -- tabs will likely be converted to spaces
no_escape _ '\n' = True
no_escape po c = if (poIsprint po) then isPrint c
                                   else isPrintableAscii c
                 ||  c >= '\x80' && po8bit po

-- | 'isPrintableAscii' tells wether a character is a printable character
-- of the ascii range.
isPrintableAscii :: Char -> Bool
isPrintableAscii c = isAscii c && isPrint c


-- | 'quoteChar' represents a special character as a string.
--   * @quoteChar '^c'@ (where @^c@ is a control character) is @"^c"@
--   * Otherwise, @quoteChar@ returns "\hex", where 'hex' is the
--     hexadecimal number of the character.
quoteChar :: Char -> String
quoteChar c
 | isControl c && isPrintableAscii cHat = ['^', cHat]
 | otherwise = sHex
 where
  cHat = chr $ (bit 6 `xor`) $ ord c
  sHex = let (q, r) = quotRem (ord c) 16
         in ['\\', intToDigit q, intToDigit r]


-- make colors and highlightings

-- | @'mark_escape' policy doc@ marks @doc@ with the appropriate
-- marking for escaped characters according to @policy@
mark_escape :: Policy -> Doc -> Doc
mark_escape po | poAltColor po  = make_invert
               | poColor po     = make_color Red
               | otherwise      = make_asciiart

-- | @'color' policy color doc@ colors @doc@ with color @color@ if
-- @policy@ is not set to use an alternative to color. In that case,
-- it makes the text bold instead.
color :: Policy -> Color -> Doc -> Doc
color po c | poAltColor po = make_bold
           | otherwise     = make_color c

make_color, make_color' :: Color -> Doc -> Doc

make_color' = with_color . set_color

-- memoized version of make_color'
make_color Blue    = make_color' Blue
make_color Red     = make_color' Red
make_color Green   = make_color' Green
make_color Cyan    = make_color' Cyan
make_color Magenta = make_color' Magenta
make_color c = make_color' c

set_color :: Color -> String
set_color Blue    = "\x1B[01;34m" -- bold blue
set_color Red     = "\x1B[01;31m" -- bold red
set_color Green   = "\x1B[01;32m" -- bold green
set_color Cyan    = "\x1B[36m"    -- light cyan
set_color Magenta = "\x1B[35m"    -- light magenta
set_color (Underline c) = "\x1B[04m" ++ set_color c

-- | @'make_asciiart' doc@ tries to make @doc@ (usually a
-- single escaped char) stand out with the help of only plain
-- ascii, i.e., no color or font style.
make_asciiart :: Doc -> Doc
make_asciiart x = unsafeBothText "[_" <> x <> unsafeBothText "_]"

-- | the string to reset the terminal's color.
reset_color :: String
reset_color = "\x1B[00m"

-- | @'with_color' color doc@ returns a colorized version of @doc@.
-- @color@ is a string that represents a color, given by 'set_color'
with_color :: String -> Doc -> Doc
with_color c =
   let c' = unsafeBothText c
       r' = unsafeBothText reset_color
   in \x -> c' <> x <> r'


-- | 'make_bold' boldens a doc.
make_bold :: Doc -> Doc
make_bold   = with_color "\x1B[01m"

-- | 'make_invert' returns an invert video version of a doc.
make_invert :: Doc -> Doc
make_invert = with_color "\x1B[07m"
\end{code}
