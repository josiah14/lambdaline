module LambdaLine.Segment
( PromptSegment
, (>+<)
, buildMainPrompt
, color
, font
, space
, style
, symbol
) where
import Control.Monad
import Data.Maybe
import Data.List as L
import LambdaLine.XTerm.Colors (Color)

type PromptSegment = IO (Maybe String)

-- Combines 2 segments into a single segment
(>+<) :: PromptSegment -> PromptSegment -> PromptSegment
(>+<) = liftM2 $ \segment0 segment1 -> case catMaybes [segment0, segment1] of []       -> Nothing
                                                                              segments -> Just $ foldl1 (++) segments

buildMainPrompt :: [PromptSegment] -> String -> String -> IO ()
buildMainPrompt segments separator promptSymbol =
  (L.foldl1
     addSegment
     segments
  ) >>= putStr . (fromMaybe "") >> putStr promptSymbol
  where addSegment = liftM2 concatSeg
          where concatSeg prompt mSeg = case mSeg of Just seg@(_:_) -> Just $ (fromMaybe "" prompt) ++ separator ++ seg
                                                     _              -> prompt

-- edit/define the color of a segment
color :: Color -> Maybe String -> PromptSegment
color xtermNum seg = return $ case seg of Just ""     -> seg
                                          Just prompt -> Just $ "%F{" ++ xtermNum ++ "}" ++ prompt ++ "%f"
                                          _           -> Nothing

font = undefined

space :: Maybe String -> PromptSegment
space mSeg = return $ case mSeg of Just ""        -> mSeg
                                   Just seg       -> Just $ seg ++ " "
                                   _              -> Nothing

style = undefined

symbol = undefined

