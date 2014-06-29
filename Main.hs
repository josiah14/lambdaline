import System.Directory
import System.Process
import Data.Text as T
import Control.Monad
import System.Exit
import Date.List
import Data.List.Split
import Control.Applicative

type ProcessResponse = IO (ExitCode, String, String)
type BranchName = IO (Maybe String)

data RepoStatus = NoChanges | ChangesToAdd | ChangesToCommit

main :: IO ()
main = do
    curDir      <- getCurrentDirectory
    maybeBranch <- getCurrentBranch
    case maybeBranch of Just branch -> print $ curDir ++ " " ++ branch
                        Nothing     -> print curDir

getCurrentBranch :: BranchName
getCurrentBranch = parseProcessResponse $ readProcessWithExitCode "git" ["rev-parse","--abbrev-ref","HEAD"] []

parseProcessResponse :: ProcessResponse -> IO (Maybe String)
parseProcessResponse processResponse = do
    (exitCode,stdOut,stdErr) <- processResponse
    case exitCode of ExitSuccess      -> return $ Just $ trim stdOut
                     ExitFailure 128  -> return Nothing
                     ExitFailure _    -> do
                       print exitCode
                       print $ stdOut ++ " " ++ stdErr
                       return Nothing
                     where trim = unpack . strip . pack

getCurrentRepoStatus :: IO (Maybe RepoStatus)
getCurrentRepoStatus = parseProcessResponse $ readProcessWithExitCode

getChangesToAdd :: IO (Maybe [String])
getChangesToAdd = liftM (fmap split "\n")  $ parseProcessResponse $ readProcessWithExitCode "git" ["add","--all","--dry-run"] []

getChangesToCommit :: IO (Maybe [String])
getChangesToCommit = liftM (fmap split "\n") $ parseProcessResponse $ readProcessWithExitCode "git" ["status","--porcelain"]

stdOutListAny :: IO (Maybe [String]) -> IO (Maybe Boolean)
stdOutListAny = liftM fmap not . null

hasChangesToAdd :: IO (Maybe Boolean)
hasChangesToAdd = stdOutListAny getChangesToAdd

hasChangesToCommit :: IO (Maybe Boolean)
hasChangesToCommit = ((&&) . <$> =<< (stdOutListAny getChangesToCommit)) =<< (not . <$> =<< hasChangesToAdd )
hasChangesToCommit = do
    changesToCommit <- getChangesToCommit
    maybeAdd        <- hasChangesToAdd
    return do
      commitsList <- changesToCommit
      addBool     <- maybeAdd
      return commitsList && not addBool

hasAnyChanges :: IO (Maybe Boolean)
