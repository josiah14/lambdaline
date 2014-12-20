{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LambdaLine.Shells.ShellSegment
( GenericSegment
, Segment(..)
, SegmentSymbol
, ShellSegment(..)
, ShellStyler
, ShellType(..)
, (&)
, buildShellPrompt
, mkShellSegment
, style
) where

import LambdaLine.XTerm.Colors(Color)
import Data.Monoid
import Data.Functor
import Control.Applicative
import Data.List(intersperse)
import Data.Maybe(catMaybes)
import LambdaLine.Segment

-- a GenericSegment is an promptType -> Segment String
type GenericSegment = ShellType -> ShellSegment String

data ShellSegment a = ShellSegment (IO (Maybe a))

instance Functor ShellSegment where
  fmap f (ShellSegment p) = ShellSegment $ fmap (fmap f) p

instance (Monoid a) => Monoid (ShellSegment a) where
  mempty = ShellSegment $ return mempty
  mappend (ShellSegment s1) (ShellSegment s2) = ShellSegment $ liftA2 (<>) s1 s2

instance Segment ShellSegment String where
  buildPrompt segments separator promptSymbol =
    let sequenceIO           = mapM (\(ShellSegment p) -> p)
        intersperseSeparator = fmap (intersperse separator . catMaybes)
        appendPromptSymbol   = fmap (\segs -> segs ++ [promptSymbol])
    in concat <$> appendPromptSymbol (intersperseSeparator $ sequenceIO segments) >>= putStr
  mkSegment = ShellSegment
  stringToSegment = ShellSegment . return . Just

data ShellType = ShellType
  { appendSpace' :: String -> String
  , bgColor' :: Color ->  String -> String
  , bold' :: String -> String
  , fgColor' :: Color ->  String -> String
  , plain' :: String -> String
  , prependSpace' :: String -> String
  , underline' :: String -> String
  }

type SegmentSymbol = ShellType -> String

type ShellStyler = String -> SegmentSymbol

-- operator to compose shell prompt styling functions together
(&) :: ShellStyler -> ShellStyler -> ShellStyler
f & g = \str shellType -> g (f str shellType) shellType

buildShellPrompt :: [GenericSegment] -> SegmentSymbol -> SegmentSymbol -> ShellType -> IO ()
buildShellPrompt segmentMakers makeSeparator makePromptSymbol shellType =
  let segments = map (\f -> f shellType) segmentMakers
      separator = makeSeparator shellType
      promptSymbol = makePromptSymbol shellType
  in buildPrompt segments separator promptSymbol

mkShellSegment :: ShellSegment String -> GenericSegment
mkShellSegment = mkFn (flip plain')
  where mkFn f seg shType = flip f shType <$> seg

style :: ShellStyler -> GenericSegment -> GenericSegment
style f makeSegment = flip f >>= \g shellType -> g <$> makeSegment shellType

