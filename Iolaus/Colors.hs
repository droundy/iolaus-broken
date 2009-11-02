module Iolaus.Colors ( Color, colorCode, spaceColor,
                       colorMeta, colorOld, colorNew ) where

import System.IO.Unsafe ( unsafePerformIO )
import Git.Plumbing ( getColorWithDefault )

data Color = Color String String

colorCode :: Color -> String
colorCode (Color c d) = unsafePerformIO $ getColorWithDefault c d

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



