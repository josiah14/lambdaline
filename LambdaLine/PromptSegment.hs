module LambdaLine.PromptSegment
( PromptSegment
, buildMainPrompt
, convertToPromptSegment
, makePromptSegment
) where
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.List as L

data PromptSegment a = PromptSegment (IO (Maybe a))

instance Functor PromptSegment where
  fmap f (PromptSegment p) = PromptSegment $ fmap (fmap f) p

instance Monoid a => Monoid (PromptSegment a) where
  mempty = PromptSegment $ return mempty
  mappend (PromptSegment p1) (PromptSegment p2) = PromptSegment $ liftA2 (<>) p1 p2

-- Exposed API Methods --

buildMainPrompt :: [PromptSegment String] -> String -> String -> IO ()
buildMainPrompt segments separator promptSymbol =
  let sequenceIO           promptSegments = sequence $ map (\(PromptSegment p) -> p) promptSegments
      intersperseSeparator ioMaybes       = fmap (\segs -> intersperse separator $ catMaybes segs) ioMaybes
      appendPromptSymbol   ioStrings      = fmap (\segs -> segs ++ [promptSymbol]) ioStrings
  in concat <$> (appendPromptSymbol $ intersperseSeparator $ sequenceIO segments) >>= putStr

convertToPromptSegment :: IO (Maybe String) -> PromptSegment String
convertToPromptSegment = PromptSegment

makePromptSegment :: String -> PromptSegment String
makePromptSegment = PromptSegment . return . Just

