module Iolaus.Colors ( Color, colorCode, resetCode, spaceColor,
                       colorMeta, colorOld, colorNew,
                       colorPlain, rainbow ) where

import System.IO.Unsafe ( unsafePerformIO )
import Git.Plumbing ( getColorWithDefault )

data Color = Color String String
             deriving ( Show, Eq )

colorCode :: Color -> String
colorCode (Color c d) = unsafePerformIO $ getColorWithDefault c d

resetCode :: String
resetCode = "\x1B[00m"

spaceColor :: Color -> Color
spaceColor (Color c d) = Color (c++".space") d'
    where d' = case words d of [d0,"bold"] -> d0++" ul"
                               _ -> d++" ul"

colorMeta :: Color
colorMeta = Color "color.diff.meta" "blue bold"

colorOld :: Color
colorOld = Color "color.diff.old" "red bold"

colorNew :: Color
colorNew = Color "color.diff.new" "green bold"

colorPlain :: Color
colorPlain = Color "" ""

rainbow :: [Color]
rainbow = [colorPlain,
           Color "c2" "blue",
           Color "c2" "magenta",
           Color "c3" "red",
           Color "c3" "yellow",
           Color "c3" "cyan",
           Color "c3" "green",
           Color "c3" "bold",
           Color "c2" "blue bold",
           Color "c2" "magenta bold",
           Color "c3" "red bold",
           Color "c3" "yellow bold",
           Color "c3" "cyan bold",
           Color "c3" "green bold"]
