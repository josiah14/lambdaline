{-# LANGUAGE MultiParamTypeClasses #-}

module LambdaLine.Segment
(Segment(..)
) where
import Data.Monoid

class (Functor f, Monoid (f a)) => Segment f a where
  buildPrompt :: [f a] -> a -> a -> IO ()
  mkSegment :: IO (Maybe a) -> f a
  stringToSegment :: a -> f a

