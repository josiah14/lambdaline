module LambdaLine.GitComm
( gitCurrentBranch
, gitPushSymbol
, gitRepositorySymbol
, gitStagedSymbol
, gitStatusSymbols
, gitUnstagedSymbol
, inGitRepository
) where
-- internal imports
import LambdaLine.Util
-- import LambdaLine.Segment (Segment, mkSegment)
-- external lib imports
import Control.Monad
import Data.Maybe
import System.Process
import LambdaLine.Shells.ShellSegment

gitCurrentBranch :: ShellSegment String
gitCurrentBranch =
  mkSegment $ parseProcessResponse $ readProcessWithExitCode "git" ["rev-parse","--abbrev-ref","HEAD"] []

gitPushSymbol :: String -> ShellSegment String
gitPushSymbol symbol = mkSegment $ hasCommitsToPush >>= calculateStatusSymbol symbol

gitRepositorySymbol :: String -> ShellSegment String
gitRepositorySymbol symbol = mkSegment $ inGitRepository >>= (\git -> if git == True
                                                                                   then return $ Just symbol
                                                                                   else return Nothing)

gitStagedSymbol :: String -> ShellSegment String
gitStagedSymbol symbol = mkSegment $ hasStagedChanges >>= calculateStatusSymbol symbol

gitStatusSymbols :: String -> String -> String -> ShellSegment String
gitStatusSymbols unstagedSym stagedSym committedSym =
  mkSegment $ getCurrentRepoStatus >>= (\mStatus ->
    case mStatus of Nothing     -> return Nothing
                    Just status -> return $ Just $ unstagedStr ++ stagedStr ++ committedStr
                                     where unstagedStr  = if unstagedChanges status then unstagedSym  else ""
                                           stagedStr    = if stagedChanges status   then stagedSym    else ""
                                           committedStr = if commitsToPush status   then committedSym else "")

gitUnstagedSymbol :: String -> ShellSegment String
gitUnstagedSymbol symbol = mkSegment $ hasUnstagedChanges >>= calculateStatusSymbol symbol

inGitRepository :: IO (Bool)
inGitRepository = return . isJust =<< (parseProcessResponse $ readProcessWithExitCode "git" ["rev-parse"] [])

-- private functions and types

data RepoStatus = RepoStatus { unstagedChanges :: Bool
                             , stagedChanges :: Bool
                             , commitsToPush :: Bool
                             } deriving Show

calculateStatusSymbol :: String -> Maybe Bool -> IO (Maybe String)
calculateStatusSymbol symbol mUnstaged = inGitRepository >>= (\inGitRepo -> if inGitRepo && fromMaybe False mUnstaged
                                                                              then return $ Just symbol
                                                                              else return Nothing)

getCurrentRepoStatus :: IO (Maybe RepoStatus)
getCurrentRepoStatus = do
  inGitRepo <- inGitRepository
  unstaged  <- hasUnstagedChanges
  staged    <- hasStagedChanges
  unpushed  <- hasCommitsToPush
  return $ if inGitRepo
           then Just $ RepoStatus (fromMaybe False unstaged) (fromMaybe False staged) (fromMaybe False unpushed)
           else Nothing

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

hasStagedChanges :: IO (Maybe Bool)
hasStagedChanges = liftM (fmap isResponseNull) $ parseProcessResponse gitResponse
  where gitResponse = readProcessWithExitCode "git" ["diff-index","--cached","--ignore-submodules","HEAD"] []

hasUnstagedChanges :: IO (Maybe Bool)
hasUnstagedChanges = liftM (fmap isResponseNull) $ parseProcessResponse gitStatus
  where gitStatus = readProcessWithExitCode "git" ["diff-files","--ignore-submodules"] []

