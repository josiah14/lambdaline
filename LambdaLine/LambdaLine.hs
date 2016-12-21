module LambdaLine.LambdaLine
( Segment
, (<>)
, buildPrompt
, exec
, fmap
, mkSegment
, mappend
, mempty
, stringToSegment
) where
import Data.Functor
import Data.Monoid
import LambdaLine.Segment
import System.Environment(getArgs)

-- This function allows the user to configure the string they want to use
-- to match up which prompt they want to build.
exec :: [(String, IO())] -> IO ()
exec prompts = getPromptType >>= selectPrompt prompts

-- internal helper functions

selectPrompt :: [(String, IO())] -> String -> IO ()
selectPrompt prompts promptType =
  let findPrompt testPromptName = promptType /= fst testPromptName
  in snd $ head $ dropWhile findPrompt prompts

getPromptType :: IO String
getPromptType = head <$> getArgs

