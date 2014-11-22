{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module LambdaLine.Shells.Zsh
( ShellPromptSegment(..)
, S.appendSpace
, bgColor
, bold
, fgColor
, underline
, S.prependSpace
, zshStyler
) where
import LambdaLine.Shells.Base as B
import LambdaLine.Shells.ShellPromptSegment as S

zshStyler :: ShellPromptStyler
zshStyler = ShellPromptStyler
  { S.appendSpace = B.appendSpace
  , bgColor = (\color -> stylePrompt (\s -> "%K{" ++ color ++ "}" ++ s ++ "%k"))
  , bold = stylePrompt (\s -> "%B" ++ s ++ "%b")
  , fgColor = (\color -> stylePrompt (\s -> "%F{" ++ color ++ "}" ++ s ++ "%f"))
  , S.prependSpace = B.prependSpace
  , underline = stylePrompt (\s -> "%U" ++ s ++ "%u")
  }

