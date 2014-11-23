module LambdaLine.Shells.Zsh
( ShellPromptSegment(..)
, ShellPromptType
, (S.&)
, appendSpace
, bgColor
, S.buildShellPrompt
, bold
, fgColor
, plain
, prependSpace
, shell
, S.style
, underline
) where
import qualified LambdaLine.Shells.Base as B
import LambdaLine.Shells.ShellPromptSegment as S
import LambdaLine.XTerm.Colors (Color)
import LambdaLine.Util (cycle3)

shell :: ShellPromptType
shell = ShellPromptType
  { appendSpace'  = B.appendSpace
  , bgColor'      = zshBgColor
  , bold'         = B.stylePrompt (\s -> "%B" ++ s ++ "%b")
  , fgColor'      = zshFgColor
  , plain'        = B.plain
  , prependSpace' = B.prependSpace
  , underline'    = B.stylePrompt (\s -> "%U" ++ s ++ "%u")
  }

appendSpace :: String -> ShellPromptType -> String
appendSpace = flip appendSpace'

bgColor :: Color -> String -> ShellPromptType -> String
bgColor = cycle3 bgColor'

bold :: String -> ShellPromptType -> String
bold = flip bold'

fgColor :: Color -> String -> ShellPromptType -> String
fgColor = cycle3 fgColor'

plain :: String -> ShellPromptType -> String
plain = flip plain'

prependSpace :: String -> ShellPromptType -> String
prependSpace = flip prependSpace'

underline :: String -> ShellPromptType -> String
underline = flip underline'

-- internal definitions to ease readability

zshBgColor :: Color -> String -> String
zshBgColor color = B.stylePrompt (\s -> "%K{" ++ color ++ "}" ++ s ++ "%k")

zshFgColor :: Color -> String -> String
zshFgColor color = B.stylePrompt (\s -> "%F{" ++ color ++ "}" ++ s ++ "%f")

