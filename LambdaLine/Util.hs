module LambdaLine.Util
( ProcessResponse
, getPromptType
, getTerminalWidth
, deleteNulls
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

deleteNulls :: [[a]] -> [[a]]
deleteNulls = L.filter $ not . L.null

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

getPromptType :: IO String
getPromptType = L.head <$> getArgs

getTerminalWidth :: IO String
getTerminalWidth = (!!1) <$> getArgs

