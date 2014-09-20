module LambdaLine.Segment
( PromptSegment
, (>+<)
, buildMainPrompt
, color
, font
, red
, space
, style
, symbol
) where
import Control.Monad
import Data.Maybe
import Data.List as L

type PromptSegment = IO (Maybe String)
type Color = String

red :: Color
red = show 009

-- Combines 2 segments into a single segment
(>+<) :: PromptSegment -> PromptSegment -> PromptSegment
(>+<) = liftM2 $ \segment0 segment1 -> Just $ foldl1 (++) $ catMaybes [segment0, segment1]

buildMainPrompt :: [PromptSegment] -> String -> IO ()
buildMainPrompt segments promptSymbol =
  (L.foldl1
     addSegment
     segments
  ) >>= putStr . (fromMaybe "") >> putStr promptSymbol
  where addSegment = liftM2 concatSeg
          where concatSeg prompt mSeg = case mSeg of Just seg@(_:_) -> Just $ (fromMaybe "" prompt) ++ "➢ " ++ seg
                                                     _              -> prompt

-- edit/define the color of a segment
color :: Color -> Maybe String -> PromptSegment
color color seg = return $ case seg of Just ""     -> seg
                                       Just prompt -> Just $ "%F{" ++ color ++ "}" ++ prompt ++ "%f"
                                       _           -> Nothing

font = undefined

space :: Maybe String -> PromptSegment
space mSeg = return $ case mSeg of Just ""        -> mSeg
                                   Just seg       -> Just $ seg ++ " "
                                   _              -> Nothing

style = undefined

symbol = undefined

