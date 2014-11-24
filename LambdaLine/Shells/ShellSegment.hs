{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LambdaLine.Shells.ShellSegment
( Segment(..)
, ShellSegment(..)
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

-- operator to compose shell prompt styling functions together
(&) :: (String -> ShellType -> String) -> (String -> ShellType -> String)
       -> String -> ShellType -> String
f & g = \str shellType -> g (f str shellType) shellType

buildShellPrompt :: [ShellType -> ShellSegment String]
                      -> (ShellType -> String)
                      -> (ShellType -> String)
                      -> ShellType
                      -> IO ()
buildShellPrompt segmentMakers makeSeparator makePromptSymbol shellType =
  let segments = map (\f -> f shellType) segmentMakers
      separator = makeSeparator shellType
      promptSymbol = makePromptSymbol shellType
  in buildPrompt segments separator promptSymbol

mkShellSegment :: ShellSegment String -> ShellType -> ShellSegment String
mkShellSegment = mkFn (flip plain')
  where mkFn f seg shType = flip f shType <$> seg

style :: (String -> ShellType -> String)
            -> (ShellType -> ShellSegment String)
            -> ShellType -> ShellSegment String
style f makeSegment = flip f >>= \g shellType -> g <$> makeSegment shellType

