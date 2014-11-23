{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LambdaLine.Shells.ShellPromptSegment
( ShellPromptSegment(..) 
, ShellPromptType(..)
, PromptSegment(..)
, (&)
, buildShellPrompt
, style
) where
  
import LambdaLine.XTerm.Colors
import Data.Monoid
import Data.Functor
import Control.Applicative
import Data.List as L
import Data.Maybe
import LambdaLine.PromptSegment

data ShellPromptType = ShellPromptType
  { appendSpace' :: String -> String
  , bgColor' :: Color ->  String -> String
  , bold' :: String -> String 
  , fgColor' :: Color ->  String -> String
  , plain' :: String -> String
  , prependSpace' :: String -> String
  , underline' :: String -> String 
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

-- operator to compose shell prompt styling functions together
(&) :: (String -> ShellPromptType -> String) -> (String -> ShellPromptType -> String)
       -> (String -> ShellPromptType -> String)
f & g = \str shellType -> g (f str shellType) shellType 

buildShellPrompt :: [(ShellPromptType -> ShellPromptSegment String)]
                      -> (ShellPromptType -> String)
                      -> (ShellPromptType -> String)
                      -> ShellPromptType
                      -> IO ()
buildShellPrompt segmentMakers makeSeparator makePromptSymbol shellType =
  let segments = map (\f -> f shellType) segmentMakers
      separator = makeSeparator shellType
      promptSymbol = makePromptSymbol shellType
  in buildPrompt segments separator promptSymbol

-- apply a style function to a shell prompt functor
-- e.g.
-- bold & fgColor red `style` gitCurrentBranch
style :: (String -> ShellPromptType -> String) -> ShellPromptSegment String
           -> (ShellPromptType -> ShellPromptSegment String)
style f segment = \shType -> (flip f) shType <$> segment

