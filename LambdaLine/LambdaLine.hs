module LambdaLine.LambdaLine
( PromptSegment
, (<$>)
, (<>)
, buildMainPrompt
, convertToPromptSegment
, fmap
, makePromptSegment
, mappend
, mempty
) where
import Data.Functor
import Data.Monoid
import LambdaLine.PromptSegment

