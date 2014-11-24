module LambdaLine.LambdaLine
( Segment
, (<$>)
, (<>)
, buildPrompt
, stringToSegment
, exec
, fmap
, mkSegment
, mappend
, mempty
) where
import Data.Functor
import Data.Monoid
import LambdaLine.Segment
import System.Environment(getArgs)

getPromptType :: IO String
getPromptType = head <$> getArgs

-- This function allows the user to configure the string they want to use
-- to match up which prompt they want to build.
exec :: [(String, IO())] -> IO ()
exec prompts = getPromptType >>= selectPrompt prompts

selectPrompt :: [(String, IO())] -> String -> IO ()
selectPrompt prompts promptType =
 let findPrompt testPromptName = promptType /= fst testPromptName
 in snd $ head $ dropWhile findPrompt prompts

