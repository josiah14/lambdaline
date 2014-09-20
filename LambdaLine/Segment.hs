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
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List as L

type PromptSegment = IO (Maybe String)

-- Combines 2 segments into a single segment
(>+<) :: PromptSegment -> PromptSegment -> PromptSegment
(>+<) = liftM2 $ \segment0 segment1 -> (++) <$> segment0 <*> segment1

buildMainPrompt :: [PromptSegment] -> String -> IO ()
buildMainPrompt segments promptSymbol =
  (L.foldl1
     addSegment
     segments
  ) >>= putStr . (fromMaybe "") >> putStr promptSymbol
  where addSegment = liftM2 concatSeg
          where concatSeg prompt mSeg = case mSeg of Just seg@(_:_) -> Just $ (fromMaybe "" prompt) ++ "➢ " ++ seg
                                                     _              -> prompt

color = undefined

font = undefined

space :: Maybe String -> PromptSegment
space mSeg = return $ case mSeg of Just seg@(_:_) -> Just $ seg ++ " "
                                   _              -> Nothing

style = undefined

symbol = undefined

