module LambdaLine.Shells.Zsh
( appendSpace
, bgColor
, bold
, fgColor
, underline
, prependSpace
) where
import LambdaLine.Shells.Base
import LambdaLine.XTerm.Colors (Color)

bgColor :: Color -> String -> String
bgColor color = stylePrompt (\s -> "%K{" ++ color ++ "}" ++ s ++ "%k")

bold :: String -> String
bold = stylePrompt (\s -> "%B" ++ s ++ "%b")

-- edit/define the foreground/font color of a segment
fgColor :: Color -> String -> String
fgColor color = stylePrompt (\s -> "%F{" ++ color ++ "}" ++ s ++ "%f")

underline :: String -> String
underline = stylePrompt (\s -> "%U" ++ s ++ "%u")

