import System.Directory
import System.Environment
import Control.Monad
import Data.List as L
import Data.Maybe
import LambdaLine.Util
import LambdaLine.GitComm

getTerminalWidth :: IO (String)
getTerminalWidth = liftM L.head getArgs

main :: IO ()
main = buildMainPrompt
         [ liftM Just getCurrentDirectory >>= space
         , (gitStatusSymbols "✚" "✎" "↑" >>= space) >+< gitCurrentBranch >+< (gitRepositorySymbol "±" >>= space)
         ]
         "λ "

buildMainPrompt :: [PromptSegment] -> String -> IO ()
buildMainPrompt segments promptSymbol =
  (L.foldl1
     addSegment
     segments
  ) >>= putStr . (fromMaybe "") >> putStr promptSymbol
  where addSegment partialPrompt additionalSegment = do
          mSeg <- additionalSegment
          prompt <- partialPrompt
          case mSeg of Nothing  -> return $ prompt
                       Just ""  -> return $ prompt
                       Just seg -> return $ Just $ (fromMaybe "" prompt) ++ "➢ " ++ seg

-- Combines 2 segments into a single segment
(>+<) :: PromptSegment -> PromptSegment -> PromptSegment
seg0 >+< seg1 = do
  mSeg0 <- seg0
  mSeg1 <- seg1
  return $ Just $ (fromMaybe "" mSeg0) ++ (fromMaybe "" mSeg1)

space :: Maybe String -> PromptSegment
space mSeg = return $ case mSeg of Nothing  -> Nothing
                                   Just ""  -> Nothing
                                   Just seg -> Just $ seg ++ " "

