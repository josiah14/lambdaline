module LambdaLine.Util
( ProcessResponse
, cycle3
, cycle4
, deleteNulls
, getPromptType
, getTerminalWidth
, isResponseNull
, parseProcessResponse
, splitOnNewLine
, stdOutListAny
, trimString
)
where
import Control.Monad
import Data.Functor((<$>))
import System.Environment(getArgs)
import Data.List as L
import Data.List.Split as SP
import Data.Text as T
import System.Exit

type ProcessResponse = IO (ExitCode, String, String)

cycle3 :: (a -> b -> c -> d) -> b -> c -> a -> d
cycle3 f y z x = f x y z

cycle4 :: (a -> b -> c -> d -> e) -> b -> c -> d -> a -> e
cycle4 f x y z w = f w x y z

deleteNulls :: [[a]] -> [[a]]
deleteNulls = L.filter $ not . L.null

getPromptType :: IO String
getPromptType = L.head <$> getArgs

getTerminalWidth :: IO String
getTerminalWidth = (!!1) <$> getArgs

isResponseNull :: String -> Bool
isResponseNull = not . L.null . splitOnNewLine . trimString

parseProcessResponse :: ProcessResponse -> IO (Maybe String)
parseProcessResponse processResponse = do
  (exitCode,stdOut,_) <- processResponse
  case exitCode of ExitSuccess    -> return $ Just $ trimString stdOut
                   ExitFailure _  -> return Nothing

splitOnNewLine :: String -> [String]
splitOnNewLine str = [ s | s <- SP.splitOn "\n" str, not . L.null $ s ]

stdOutListAny :: IO (Maybe [String]) -> IO (Maybe Bool)
stdOutListAny = liftM (fmap $ not . L.null)

trimString :: String -> String
trimString = unpack . strip . pack

