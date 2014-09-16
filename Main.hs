import System.Directory
import System.Process
import Data.Text as T
import Control.Monad
import System.Exit
import Data.List as L
import Data.List.Split as SP
import Data.Maybe
import System.Console.Terminal.Size as TS

type ProcessResponse = IO (ExitCode, String, String)
type BranchName = String
type Segment = IO String

data RepoStatus = RepoStatus { unstagedChages :: Bool
                             , stagedChanges :: Bool
                             , commitsToPush :: Bool
                             } deriving Show

terminalWidth :: IO String
terminalWidth = liftM (show . width . fromJust) TS.size

main :: IO ()
main =
  (L.foldr1
     addSegment
     [ liftM (fromMaybe "") getCurrentBranch
     , liftM (\status -> if isNothing status then "" else show $ fromJust status) getCurrentRepoStatus
     , getCurrentDirectory
     ]
  ) >>= print
  where addSegment = liftM2 (++)

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

