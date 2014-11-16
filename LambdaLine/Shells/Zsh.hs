{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module LambdaLine.Shells.Zsh
( ZshPromptSegment
, Monoid(..)
, Functor(..)
, PromptSegment(..)
, appendSpace
, bgColor
, bold
, fgColor
, underline
, prependSpace
) where
import LambdaLine.PromptSegment
import Data.List as L
import Control.Applicative
import Data.Maybe
import Data.Monoid
import LambdaLine.Shells.Base
import LambdaLine.XTerm.Colors (Color)

data ZshPromptSegment a = ZshPromptSegment (IO (Maybe a))

instance Monoid a => Monoid (ZshPromptSegment a) where
  mempty = ZshPromptSegment $ return mempty
  mappend (ZshPromptSegment p1) (ZshPromptSegment p2) = ZshPromptSegment $ liftA2 (<>) p1 p2

instance Functor ZshPromptSegment where
  fmap f (ZshPromptSegment p) = ZshPromptSegment $ fmap (fmap f) p

instance PromptSegment ZshPromptSegment String where
  buildPrompt segments separator promptSymbol =
    let sequenceIO           promptSegments = sequence $ map (\(ZshPromptSegment p) -> p) promptSegments
        intersperseSeparator ioMaybes       = fmap (\segs -> intersperse separator $ catMaybes segs) ioMaybes
        appendPromptSymbol   ioStrings      = fmap (\segs -> segs ++ [promptSymbol]) ioStrings
    in concat <$> (appendPromptSymbol $ intersperseSeparator $ sequenceIO segments) >>= putStr
  convertToPromptSegment = ZshPromptSegment
  makePromptSegment = ZshPromptSegment . return . Just

bgColor :: Color -> String -> String
bgColor color = stylePrompt (\s -> "%K{" ++ color ++ "}" ++ s ++ "%k")

bold :: String -> String
bold = stylePrompt (\s -> "%B" ++ s ++ "%b")

-- edit/define the foreground/font color of a segment
fgColor :: Color -> String -> String
fgColor color = stylePrompt (\s -> "%F{" ++ color ++ "}" ++ s ++ "%f")

underline :: String -> String
underline = stylePrompt (\s -> "%U" ++ s ++ "%u")

