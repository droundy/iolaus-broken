
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
module Arcs.Printer
    (Printable(..), Doc(Doc,unDoc), Printers, Printers'(..), Printer, Color(..),
                hPutDoc,     hPutDocLn,     putDoc,     putDocLn,
                hPutDocWith, hPutDocLnWith, putDocWith, putDocLnWith,
                renderString, renderStringWith, renderPS, renderPSWith,
                renderPSs, renderPSsWith, lineColor,
                prefix, colorText, invisibleText, hiddenText, hiddenPrefix, userchunk, text,
                printable, wrap_text,
                blueText, redText, greenText, magentaText, cyanText,
                unsafeText, unsafeBoth, unsafeBothText, unsafeChar,
                invisiblePS, packedString, unsafePackedString, userchunkPS,
                simplePrinters, invisiblePrinter, simplePrinter,
                doc, empty, (<>), (<?>), (<+>), ($$), vcat, vsep, hcat,
                minus, newline, plus, space, backslash, lparen, rparen,
                parens,
                errorDoc,
               ) where

import Data.List (intersperse)
import System.IO (Handle, stdout, hPutStr)
import qualified Data.ByteString as B (ByteString, hPut, concat)
import qualified Data.ByteString.Char8 as BC (unpack, pack, singleton)

-- | A 'Printable' is either a String, a packed string, or a chunk of
-- text with both representations.
data Printable = S !String
               | PS !B.ByteString
               | Both !String !B.ByteString

-- | 'space_p' is the 'Printable' representation of a space.
space_p :: Printable
space_p   = Both " "  (BC.singleton ' ')

-- | 'newline_p' is the 'Printable' representation of a newline.
newline_p :: Printable
newline_p = S "\n"

-- | Minimal 'Doc's representing the common characters 'space', 'newline'
-- 'minus', 'plus', and 'backslash'.
space, newline, plus, minus, backslash :: Doc
space     = unsafeBoth " "  (BC.singleton ' ')
newline   = unsafeChar '\n'
minus     = unsafeBoth "-"  (BC.singleton '-')
plus      = unsafeBoth "+"  (BC.singleton '+')
backslash = unsafeBoth "\\" (BC.singleton '\\')

-- | 'lparen' is the 'Doc' that represents @\"(\"@
lparen :: Doc
lparen = unsafeBoth  "(" (BC.singleton '(')

-- | 'rparen' is the 'Doc' that represents @\")\"@
rparen :: Doc
rparen = unsafeBoth ")" (BC.singleton ')')

-- | @'parens' doc@ returns a 'Doc' with the content of @doc@ put within
-- a pair of parenthesis.
parens :: Doc -> Doc
parens d = lparen <> d <> rparen

errorDoc :: Doc -> a
errorDoc = error . renderStringWith simplePrinters'


-- | 'putDocWith' puts a doc on stdout using the given printer.
putDocWith :: Printers -> Doc -> IO ()
putDocWith prs = hPutDocWith prs stdout

-- | 'putDocLnWith' puts a doc, followed by a newline on stdout using
-- the given printer.
putDocLnWith :: Printers -> Doc -> IO ()
putDocLnWith prs = hPutDocLnWith prs stdout


-- | 'putDoc' puts a doc on stdout using the simple printer 'simplePrinters'.
putDoc :: Doc -> IO ()
-- | 'putDocLn' puts a doc, followed by a newline on stdout using
-- 'simplePrinters'
putDocLn :: Doc -> IO ()
putDoc = hPutDoc stdout
putDocLn = hPutDocLn stdout

-- | 'hputDocWith' puts a doc on the given handle using the given printer.
hPutDocWith :: Printers -> Handle -> Doc -> IO ()
-- | 'hputDocLnWith' puts a doc, followed by a newline on the given
-- handle using the given printer.
hPutDocLnWith :: Printers -> Handle -> Doc -> IO ()

hPutDocWith prs h d = hPrintPrintables h (renderWith (prs h) d)
hPutDocLnWith prs h d = hPutDocWith prs h (d <?> newline)

-- |'hputDoc' puts a doc on the given handle using 'simplePrinters'
hPutDoc :: Handle -> Doc -> IO ()
-- 'hputDocLn' puts a doc, followed by a newline on the given handle using
-- 'simplePrinters'.
hPutDocLn :: Handle -> Doc -> IO ()
hPutDoc = hPutDocWith simplePrinters
hPutDocLn = hPutDocLnWith simplePrinters

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
                           invisibleP :: !Printer,
                           hiddenP :: !Printer,
                           userchunkP :: !Printer,
                           defP :: !Printer,
                           lineColorT :: !(Color -> Doc -> Doc),
                           lineColorS :: !([Printable] -> [Printable])
                          }
type Printer = Printable -> St -> Document

data Color = Blue | Red | Green | Cyan | Magenta

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

-- | renders a 'Doc' into 'B.ByteString' with control codes for the
-- special features of the Doc. See also 'readerString'.
renderPS :: Doc -> B.ByteString
renderPS = renderPSWith simplePrinters'

-- | renders a 'Doc' into a list of 'PackedStrings', one for each line.
renderPSs :: Doc -> [B.ByteString]
renderPSs = renderPSsWith simplePrinters'

-- | renders a doc into a 'B.ByteString' using a given set of printers.
renderPSWith :: Printers' -> Doc -> B.ByteString
renderPSWith prs d = B.concat $ renderPSsWith prs d

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

lineColor :: Color -> Doc -> Doc
lineColor c d = Doc $ \st -> case lineColorT (printers st) c d of
                             Doc d' -> d' st

hiddenPrefix :: String -> Doc -> Doc
hiddenPrefix s (Doc d) =
    Doc $ \st -> let pr = printers st
                     p = S (renderStringWith pr $ hiddenText s)
                     st' = st { current_prefix = current_prefix st . (p:) }
                 in case d st' of
                      Document d'' -> Document $ (p:) . d''
                      Empty -> Empty

-- | 'unsafeBoth' builds a Doc from a 'String' and a 'B.ByteString' representing
-- the same text, but does not check that they do.
unsafeBoth :: String -> B.ByteString -> Doc
unsafeBoth s ps = Doc $ simplePrinter (Both s ps)

-- | 'unsafeBothText' builds a 'Doc' from a 'String'. The string is stored in the
-- Doc as both a String and a 'B.ByteString'.
unsafeBothText :: String -> Doc
unsafeBothText s = Doc $ simplePrinter (Both s (BC.pack s))

-- | 'packedString' builds a 'Doc' from a 'B.ByteString' using 'printable'
packedString :: B.ByteString -> Doc
-- | 'unsafePackedString' builds a 'Doc' from a 'B.ByteString' using 'simplePrinter'
unsafePackedString :: B.ByteString -> Doc
-- | 'invisiblePS' creates a 'Doc' with invisible text from a 'B.ByteString'
invisiblePS :: B.ByteString -> Doc
-- | 'userchunkPS' creates a 'Doc' representing a user chunk from a 'B.ByteString'.
userchunkPS :: B.ByteString -> Doc
packedString = printable . PS
unsafePackedString = Doc . simplePrinter . PS
invisiblePS = invisiblePrintable . PS
userchunkPS = userchunkPrintable . PS

-- | 'unsafeChar' creates a Doc containing just one character.
unsafeChar :: Char -> Doc
unsafeChar = unsafeText . (:"")

-- | 'text' creates a 'Doc' from a @String@, using 'printable'.
text :: String -> Doc
-- | 'unsafeText' creates a 'Doc' from a 'String', using 'simplePrinter' directly
unsafeText :: String -> Doc
-- | 'invisibleText' creates a 'Doc' containing invisible text from a @String@
invisibleText :: String -> Doc
-- | 'hiddenText' creates a 'Doc' containing hidden text from a @String@
hiddenText :: String -> Doc
-- | 'userchunk' creates a 'Doc' containing a user chunk from a @String@
userchunk :: String -> Doc
-- | 'blueText' creates a 'Doc' containing blue text from a @String@
blueText, redText, greenText, magentaText, cyanText :: String -> Doc
text = printable . S
unsafeText = Doc . simplePrinter . S
invisibleText = invisiblePrintable . S
hiddenText = hiddenPrintable . S
userchunk = userchunkPrintable . S
blueText = colorText Blue
redText = colorText Red
greenText = colorText Green
magentaText = colorText Magenta
cyanText = colorText Cyan

-- | 'colorText' creates a 'Doc' containing colored text from a @String@
colorText :: Color -> String -> Doc
colorText c = mkColorPrintable c . S

-- | @'wrap_text' n s@ is a 'Doc' representing @s@ line-wrapped at 'n' characters
wrap_text :: Int -> String -> Doc
wrap_text n s =
    vcat $ map text $ reverse $ "": (foldl add_to_line [] $ words s)
  where add_to_line [] a = [a]
        add_to_line ("":d) a = (a:d)
        add_to_line (l:ls) new | length l + length new > n = new:l:ls
        add_to_line (l:ls) new = (l ++ " " ++ new):ls

-- | 'printable x' creates a 'Doc' from any 'Printable'.
printable, invisiblePrintable, hiddenPrintable, userchunkPrintable :: Printable -> Doc
printable x = Doc $ \st -> defP (printers st) x st

mkColorPrintable :: Color -> Printable -> Doc
mkColorPrintable c x = Doc $ \st -> colorP (printers st) c x st
invisiblePrintable x = Doc $ \st -> invisibleP (printers st) x st
hiddenPrintable x = Doc $ \st -> hiddenP (printers st) x st
userchunkPrintable x = Doc $ \st -> userchunkP (printers st) x st

-- | 'simplePrinters' is a 'Printers' which uses the set 'simplePriners\'' on any
-- handle.
simplePrinters :: Printers
simplePrinters _ = simplePrinters'

-- | A set of default printers suitable for any handle. Does not use color.
simplePrinters' :: Printers'
simplePrinters'  = Printers { colorP = const simplePrinter,
                              invisibleP = simplePrinter,
                              hiddenP = invisiblePrinter,
                              userchunkP = simplePrinter,
                              defP = simplePrinter,
                              lineColorT = const id,
                              lineColorS = id
                            }

-- | 'simplePrinter' is the simplest 'Printer': it just concatenates together
-- the pieces of the 'Doc'
simplePrinter :: Printer
-- | 'invisiblePrinter' is the 'Printer' for hidden text. It just replaces
-- the document with 'empty'.  It's useful to have a printer that doesn't
-- actually do anything because this allows you to have tunable policies,
-- for example, only printing some text if it's to the terminal, but not
-- if it's to a file or vice-versa.
invisiblePrinter :: Printer
simplePrinter x = unDoc $ doc (\s -> x:s)
invisiblePrinter _ = unDoc empty

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
                                         Document bf -> sf (newline_p:pf (bf s)))
                        where pf = current_prefix st
                              sf = lineColorS $ printers st

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

