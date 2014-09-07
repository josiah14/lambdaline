import System.Directory
import System.Process
import Data.Text as T
import Control.Monad
import System.Exit
import Data.List as L
import Data.List.Split as SP
import Control.Applicative
import Data.Maybe

type ProcessResponse = IO (ExitCode, String, String)
type BranchName = IO (Maybe String)

data RepoStatus = RepoStatus { unstagedChages :: Bool
                             , stagedChanges :: Bool
                             , commitsToPush :: Bool
                             } deriving Show

main :: IO ()
main =
  (L.foldr1
     addSegment
     [ liftM (fromMaybe "") getCurrentBranch
     , liftM show getCurrentRepoStatus
     , getCurrentDirectory
     ]
  ) >>= print
  where addSegment = liftM2 (++)

getCurrentBranch :: BranchName
getCurrentBranch = parseProcessResponse $ readProcessWithExitCode "git" ["rev-parse","--abbrev-ref","HEAD"] []

parseProcessResponse :: ProcessResponse -> IO (Maybe String)
parseProcessResponse processResponse = do
  (exitCode,stdOut,stdErr) <- processResponse
  case exitCode of ExitSuccess      -> return $ Just $ trimString stdOut
                   ExitFailure 128  -> return Nothing
                   ExitFailure _    -> do
                     print exitCode
                     print $ stdOut ++ " " ++ stdErr
                     return Nothing

getCurrentRepoStatus :: IO (RepoStatus)
getCurrentRepoStatus = do
  unstaged <- hasUnstagedChanges
  staged   <- hasStagedChanges
  unpushed <- hasCommitsToPush
  return $ RepoStatus (fromMaybe False unstaged) (fromMaybe False staged) (fromMaybe False unpushed)


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
    of Nothing                                       -> return Nothing
       Just []                                       -> return $ Just False
       Just [_]                                      -> return $ Just True -- This case is for a new repository with the first commit in local but not yet pushed.
       Just [ latestRemoteCommit, latestLocalCommit] -> return $ Just $ latestRemoteCommit /= latestLocalCommit
  where gitRemoteRefDiff = readProcessWithExitCode "git" ["rev-parse", "@{u}", "HEAD"] []

stdOutListAny :: IO (Maybe [String]) -> IO (Maybe Bool)
stdOutListAny = liftM (fmap $ not . L.null)

deleteNulls :: [[a]] -> [[a]]
deleteNulls = L.filter $ not . L.null

trimString :: String -> String
trimString = unpack . strip . pack

isResponseNull :: String -> Bool
isResponseNull = not . L.null . splitOnNewLine . trimString

