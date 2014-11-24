module LambdaLine.Shells.Base
( appendSpace
, mkSegment
, plain
, prependSpace
, stringToSegment
, stylePrompt
) where
import LambdaLine.Segment

appendSpace :: String -> String
appendSpace = stylePrompt (++ " ")

plain :: String -> String
plain = id

prependSpace :: String -> String
prependSpace = stylePrompt (' ':)

stylePrompt :: (String -> String) -> String -> String
stylePrompt f prompt = if null prompt
                       then prompt
                       else f prompt

