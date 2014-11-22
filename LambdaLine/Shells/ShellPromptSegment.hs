{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LambdaLine.Shells.ShellPromptSegment
( ShellPromptSegment(..) 
, ShellPromptStyler(..)
, PromptSegment(..)
, cycle3
) where
  
import LambdaLine.XTerm.Colors
import Data.Monoid
import Data.Functor
import Control.Applicative
import Data.List as L
import Data.Maybe
import LambdaLine.PromptSegment

data ShellPromptStyler = ShellPromptStyler
  { appendSpace :: String -> String
  , bgColor :: Color ->  String -> String
  , bold :: String -> String 
  , fgColor :: Color ->  String -> String
  , prependSpace :: String -> String
  , underline :: String -> String 
  }

data ShellPromptSegment a = ShellPromptSegment (IO (Maybe a))

instance (Monoid a) => Monoid (ShellPromptSegment a) where
  mempty = ShellPromptSegment $ return mempty
  mappend (ShellPromptSegment s1) (ShellPromptSegment s2) = ShellPromptSegment $ liftA2 (<>) s1 s2

instance Functor ShellPromptSegment where
  fmap f (ShellPromptSegment p) = ShellPromptSegment $ fmap (fmap f) p

instance PromptSegment ShellPromptSegment String where
  buildPrompt segments separator promptSymbol =
    let sequenceIO           promptSegments = sequence $ map (\(ShellPromptSegment p) -> p) promptSegments
        intersperseSeparator ioMaybes       = fmap (\segs -> intersperse separator $ catMaybes segs) ioMaybes
        appendPromptSymbol   ioStrings      = fmap (\segs -> segs ++ [promptSymbol]) ioStrings
    in concat <$> (appendPromptSymbol $ intersperseSeparator $ sequenceIO segments) >>= putStr
  convertToPromptSegment = ShellPromptSegment
  makePromptSegment = ShellPromptSegment . return . Just

-- helper functions
cycle3 :: (a -> b -> c -> d) -> b -> c -> a -> d
cycle3 = (flip .) . flip

