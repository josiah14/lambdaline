module LambdaLine.PromptSegment
( PromptSegment
, (>+<)
, bgColor
, bold
, buildMainPrompt
, fgColor
, makePromptSegment
, prependSpace
, space
, underline
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

bgColor :: Color -> Maybe String -> Maybe String
bgColor color seg = case seg of Just ""     -> seg
                                Just prompt -> Just $ "%K{" ++ color ++ "}" ++ prompt ++ "%k"
                                _           -> Nothing

bold :: Maybe String -> Maybe String
bold mSeg = case mSeg of Just ""  -> mSeg
                         Just seg -> Just $ "%B" ++ seg ++ "%b"
                         _        -> Nothing

buildMainPrompt :: [PromptSegment] -> PromptSegment -> PromptSegment -> IO ()
buildMainPrompt segments separator promptSymbol =
  (L.foldl1
     addSegment
     segments
  ) >>= putStr . (fromMaybe "") >> promptSymbol >>= putStr . (fromMaybe "" )
  where addSegment =
          liftM3 concatSeg separator
                where concatSeg sep prompt mSeg =
                        case mSeg of Just seg@(_:_) -> Just $ (fromMaybe "" prompt) ++ (fromMaybe "" sep) ++ seg
                                     _              -> prompt

-- edit/define the foreground/font color of a segment
fgColor :: Color -> Maybe String -> Maybe String
fgColor color seg = case seg of Just ""     -> seg
                                Just prompt -> Just $ "%F{" ++ color ++ "}" ++ prompt ++ "%f"
                                _           -> Nothing

makePromptSegment :: String -> PromptSegment
makePromptSegment = return . Just

prependSpace :: Maybe String -> Maybe String
prependSpace mSeg = case mSeg of Just ""        -> mSeg
                                 Just seg       -> Just $ ' ':seg
                                 _              -> Nothing

space :: Maybe String -> Maybe String
space mSeg = case mSeg of Just ""        -> mSeg
                          Just seg       -> Just $ seg ++ " "
                          _              -> Nothing

underline :: Maybe String -> Maybe String
underline mSeg = case mSeg of Just ""  -> mSeg
                              Just seg -> Just $ "%U" ++ seg ++ "%u"
                              _        -> Nothing

