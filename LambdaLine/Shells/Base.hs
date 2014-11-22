module LambdaLine.Shells.Base
( appendSpace
, convertToPromptSegment
, makePromptSegment
, prependSpace
, stylePrompt
) where
import LambdaLine.PromptSegment

appendSpace :: String -> String
appendSpace = stylePrompt (++ " ")

prependSpace :: String -> String
prependSpace = stylePrompt (' ':)

stylePrompt :: (String -> String) -> String -> String
stylePrompt f prompt = if null prompt
                       then prompt
                       else f prompt

