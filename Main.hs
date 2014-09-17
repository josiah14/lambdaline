import System.Directory
import System.Process
import System.Environment
import Data.Text as T
import Control.Monad
import System.Exit
import Data.List as L
import Data.List.Split as SP
import Data.Maybe
import System.Console.Terminal.Size as TS
import Control.Applicative

type ProcessResponse = IO (ExitCode, String, String)
type BranchName = String
type Segment = IO String

data RepoStatus = RepoStatus { unstagedChanges :: Bool
                             , stagedChanges :: Bool
                             , commitsToPush :: Bool
                             } deriving Show

getTerminalWidth :: IO (String)
getTerminalWidth = liftM L.head getArgs

gitRepositorySymbol :: String -> IO (Maybe String)
gitRepositorySymbol symbol = inGitRepository >>= (\git -> if git == True then return $ Just symbol else return Nothing)

main :: IO ()
main =
  (L.foldr1
     addSegment
     [ gitRepositorySymbol "±"
     , getCurrentBranch
     , gitStatusSymbols "✚" "✎" "↑"
     , liftM Just getCurrentDirectory
     ]
  ) >>= putStr . (fromMaybe "") >> putStr " "
  where addSegment additionalSegment partialPrompt = do
          seg <- additionalSegment
          prompt <- partialPrompt
          case seg of Nothing  -> return $ prompt
                      Just seg -> return $ Just $ (fromMaybe "" prompt) ++ " " ++ seg

gitStatusSymbols :: String -> String -> String -> IO (Maybe String)
gitStatusSymbols unstagedSym stagedSym committedSym = do
  maybeStatus <- getCurrentRepoStatus
  case maybeStatus of Just status -> return $ Just $ unstagedStr ++ stagedStr ++ committedStr
                                       where unstagedStr  = if unstagedChanges status then unstagedSym  else ""
                                             stagedStr    = if stagedChanges status   then stagedSym    else ""
                                             committedStr = if commitsToPush status   then committedSym else ""
                      Nothing     -> return Nothing

getCurrentBranch :: IO (Maybe BranchName)
getCurrentBranch = parseProcessResponse $ readProcessWithExitCode "git" ["rev-parse","--abbrev-ref","HEAD"] []

inGitRepository :: IO (Bool)
inGitRepository = return . isJust =<< (parseProcessResponse $ readProcessWithExitCode "git" ["rev-parse"] [])

parseProcessResponse :: ProcessResponse -> IO (Maybe String)
parseProcessResponse processResponse = do
  (exitCode,stdOut,stdErr) <- processResponse
  case exitCode of ExitSuccess      -> return $ Just $ trimString stdOut
                   ExitFailure 128  -> return Nothing
                   ExitFailure _    -> do
                     print exitCode
                     print $ stdOut ++ " " ++ stdErr
                     return Nothing

getCurrentRepoStatus :: IO (Maybe RepoStatus)
getCurrentRepoStatus = do
  inGitRepo <- inGitRepository
  unstaged  <- hasUnstagedChanges
  staged    <- hasStagedChanges
  unpushed  <- hasCommitsToPush
  return $ if inGitRepo
           then Just $ RepoStatus (fromMaybe False unstaged) (fromMaybe False staged) (fromMaybe False unpushed)
           else Nothing


splitOnNewLine :: String -> [String]
splitOnNewLine str = [ s | s <- SP.splitOn "\n" str, not . L.null $ s ]

hasStagedChanges:: IO (Maybe Bool)
hasStagedChanges = liftM (fmap isResponseNull) $ parseProcessResponse gitResponse
  where gitResponse = readProcessWithExitCode "git" ["diff-index","--cached","--ignore-submodules","HEAD"] []

hasUnstagedChanges :: IO (Maybe Bool)
hasUnstagedChanges = liftM (fmap isResponseNull) $ parseProcessResponse gitStatus
  where gitStatus = readProcessWithExitCode "git" ["diff-files","--ignore-submodules"] []

hasCommitsToPush :: IO (Maybe Bool)
hasCommitsToPush = do
  latestCommits <- liftM (fmap $ deleteNulls . splitOnNewLine) $ parseProcessResponse gitRemoteRefDiff
  case latestCommits
    of Nothing                                      -> return Nothing
       Just []                                      -> return $ Just False
       Just [_]                                     -> return $ Just True -- This case is for a new repository with the first commit in local but not yet pushed.
       Just [latestRemoteCommit, latestLocalCommit] -> return . Just $ latestRemoteCommit /= latestLocalCommit
       _                                            -> return Nothing
  where gitRemoteRefDiff = readProcessWithExitCode "git" ["rev-parse", "@{u}", "HEAD"] []

stdOutListAny :: IO (Maybe [String]) -> IO (Maybe Bool)
stdOutListAny = liftM (fmap $ not . L.null)

deleteNulls :: [[a]] -> [[a]]
deleteNulls = L.filter $ not . L.null

trimString :: String -> String
trimString = unpack . strip . pack

isResponseNull :: String -> Bool
isResponseNull = not . L.null . splitOnNewLine . trimString

