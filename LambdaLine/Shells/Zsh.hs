module LambdaLine.Shells.Zsh
( GenericSegment
, ShellSegment
, ShellType
, (S.&)
, appendSpace
, bgColor
, S.buildShellPrompt
, bold
, fgColor
, S.mkShellSegment
, plain
, prependSpace
, shell
, S.style
, underline
) where
import qualified LambdaLine.Shells.Base as B
import LambdaLine.Shells.ShellSegment as S
import LambdaLine.XTerm.Colors (Color)
import LambdaLine.Util (cycle3)

shell :: ShellType
shell = ShellType
  { appendSpace'  = B.appendSpace
  , bgColor'      = zshBgColor
  , bold'         = B.stylePrompt (\s -> "%B" ++ s ++ "%b")
  , fgColor'      = zshFgColor
  , plain'        = B.plain
  , prependSpace' = B.prependSpace
  , underline'    = B.stylePrompt (\s -> "%U" ++ s ++ "%u")
  }

-- internal definitions to ease readability

zshBgColor :: Color -> String -> String
zshBgColor color = B.stylePrompt (\s -> "%K{" ++ color ++ "}" ++ s ++ "%k")

zshFgColor :: Color -> String -> String
zshFgColor color = B.stylePrompt (\s -> "%F{" ++ color ++ "}" ++ s ++ "%f")

-- exposed methods

appendSpace :: String -> ShellType -> String
appendSpace = flip appendSpace'

bgColor :: Color -> String -> ShellType -> String
bgColor = cycle3 bgColor'

bold :: String -> ShellType -> String
bold = flip bold'

fgColor :: Color -> String -> ShellType -> String
fgColor = cycle3 fgColor'

plain :: String -> ShellType -> String
plain = flip plain'

prependSpace :: String -> ShellType -> String
prependSpace = flip prependSpace'

underline :: String -> ShellType -> String
underline = flip underline'

