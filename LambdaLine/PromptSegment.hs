{-# LANGUAGE MultiParamTypeClasses #-}

module LambdaLine.PromptSegment
(PromptSegment(..)
) where
import Data.Monoid

class (Functor f, Monoid (f a)) => PromptSegment f a where
  buildPrompt :: [f a] -> a -> a -> IO ()
  convertToPromptSegment :: IO (Maybe a) -> f a
  makePromptSegment :: a -> f a

