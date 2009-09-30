{-# OPTIONS -fno-warn-orphans #-}
module Iolaus.ColorPrinter ( errorDoc, traceDoc, assertDoc, fancyPrinters ) where

import Debug.Trace ( trace )
import System.IO ( stderr )
import Iolaus.Printer (Printer, Printers, Printers'(..), Printable(..), Color(..),
                invisiblePrinter, (<>), (<?>), Doc(Doc,unDoc), unsafeBothText, simplePrinter, hcat,
                unsafeText, unsafeChar, space, unsafePackedString,
                renderStringWith, prefix )
import Data.Char ( isAscii, isPrint, isSpace, isControl, ord, chr, intToDigit )
import Data.Bits ( bit, xor )
import System.Environment ( getEnv )
import qualified Data.ByteString.Char8 as BC (unpack, any, last, spanEnd)
import qualified Data.ByteString       as B (null, init)
import System.IO.Unsafe ( unsafePerformIO )
import System.IO ( hIsTerminalDevice, Handle )

dollar, cr :: Doc
dollar = unsafeBothText "$"
cr     = unsafeBothText "\r"

errorDoc :: Doc -> a
errorDoc = error . show

traceDoc :: Doc -> a -> a
traceDoc d = trace (show d)

assertDoc :: Maybe Doc -> a -> a
assertDoc Nothing x = x
assertDoc (Just e) _ = errorDoc e

instance Show Doc where
    show = renderStringWith (fancyPrinters stderr)

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
                             invisibleP = invisiblePrinter,
                             hiddenP = colorPrinter (getPolicy h) Green,
                             userchunkP  = userchunkPrinter (getPolicy h),
                             defP       = escapePrinter (getPolicy h),
                             lineColorT = lineColorTrans (getPolicy h),
                             lineColorS = lineColorSuffix (getPolicy h)
                           }

-- | @'lineColorTrans' policy@ tries to color a Doc, according to policy po.
-- That is, if @policy@ has @poLineColor@ set, then colors the line, otherwise
-- does nothing.
lineColorTrans :: Policy -> Color -> Doc -> Doc
lineColorTrans po | poLineColor po = \c d -> prefix (set_color c) d <?> unsafeBothText reset_color
                  | otherwise      = const id

lineColorSuffix :: Policy -> [Printable] -> [Printable]
lineColorSuffix po | poLineColor po = \d -> S reset_color : d
                   | otherwise      = id

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
color po | poAltColor po = \_ -> make_bold
         | otherwise     = make_color

make_color, make_color' :: Color -> Doc -> Doc

make_color' = with_color . set_color

-- memoized version of make_color'
make_color Blue    = make_color' Blue
make_color Red     = make_color' Red
make_color Green   = make_color' Green
make_color Cyan    = make_color' Cyan
make_color Magenta = make_color' Magenta

set_color :: Color -> String
set_color Blue    = "\x1B[01;34m" -- bold blue
set_color Red     = "\x1B[01;31m" -- bold red
set_color Green   = "\x1B[01;32m" -- bold green
set_color Cyan    = "\x1B[36m"    -- light cyan
set_color Magenta = "\x1B[35m"    -- light magenta

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
-- | 'make_invert' returns an invert video version of a doc.
make_invert :: Doc -> Doc
make_bold   = with_color "\x1B[01m"
make_invert = with_color "\x1B[07m"
