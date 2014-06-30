import System.Directory
import System.Process
import Data.Text as T
import Control.Monad
import System.Exit
import Data.List as L
import Data.List.Split as SP
import Control.Applicative

type ProcessResponse = IO (ExitCode, String, String)
type BranchName = IO (Maybe String)

data RepoStatus = NoChanges | ChangesToAdd | ChangesToCommit | ChangesToAddAndCommit deriving Show

-- main :: IO ()
-- main = do
--     curDir      <- getCurrentDirectory
--     maybeBranch <- getCurrentBranch
--     case maybeBranch of Just branch -> print $ curDir ++ " " ++ branch
--                         Nothing     -> print curDir

main :: IO ()
main = do
    curDir <- getCurrentDirectory
    (L.foldr1 (addSegment) [] [getCurrentBranch,getCurrentRepoStatus]) >>= print
    where addSegment seg prompt = liftM (fmap ((++) prompt)) (liftM (fmap $ L.concat . show) seg)


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
getCurrentRepoStatus = do
    maybeChanges      <- hasChanges
    maybeCommitAndAdd <- hasChangesToAddAndCommit
    maybeAddOnly      <- hasChangesToCommitButNotAdd
    maybeCommitOnly   <- hasChangesToAddButNotCommit
    case (maybeChanges, maybeCommitAndAdd, maybeAddOnly, maybeCommitOnly) of
      (Just False,_,_,_) -> return $ Just NoChanges
      (_,_,Just True,_)  -> return $ Just ChangesToAdd
      (_,_,_,Just True)  -> return $ Just ChangesToCommit
      (_,Just True,_,_)  -> return $ Just ChangesToAddAndCommit
      _                   -> return Nothing

splitOnNewline :: String -> [String]
splitOnNewline str = [ s | s <- SP.splitOn "\n" str, not . L.null $ s ]

getChangesToAdd :: IO (Maybe [String])
getChangesToAdd = liftM (fmap splitOnNewline) $ parseProcessResponse $ gitAddDryRun
                where gitAddDryRun = readProcessWithExitCode "git" ["add","--all","--dry-run"] []

getAllChanges :: IO (Maybe [String])
getAllChanges = liftM (fmap splitOnNewline) $ parseProcessResponse $ gitStatus
                   where gitStatus = readProcessWithExitCode "git" ["status","--porcelain"] []

stdOutListAny :: IO (Maybe [String]) -> IO (Maybe Bool)
stdOutListAny = liftM (fmap $ not . L.null)

hasChangesToAdd :: IO (Maybe Bool)
hasChangesToAdd = stdOutListAny getChangesToAdd

hasChanges :: IO (Maybe Bool)
hasChanges = stdOutListAny getAllChanges

hasChangesToCommitButNotAdd :: IO (Maybe Bool)
hasChangesToCommitButNotAdd = do
    maybeChanges <- hasChanges
    maybeAdd     <- hasChangesToAdd
    return $ (&&) <$> maybeChanges <*> (not <$> maybeAdd)

-- length is not enough to determine if add changes are equal to all changes
hasChangesToAddButNotCommit :: IO (Maybe Bool)
hasChangesToAddButNotCommit = do
    maybeHasChanges <- hasChanges
    maybeAdd        <- getChangesToAdd
    maybeChanges    <- getAllChanges
    return $ (&&) <$> maybeHasChanges <*> ((==) <$> (L.length <$> maybeChanges) <*> (L.length <$> maybeAdd))

hasChangesToAddAndCommit :: IO (Maybe Bool)
hasChangesToAddAndCommit = do
    maybeHasChanges <- hasChanges
    maybeCommitNotAdd <- hasChangesToCommitButNotAdd
    maybeAddNotCommit <- hasChangesToAddButNotCommit
    return $ (&&) <$> maybeHasChanges <*> ((&&) <$> (not <$> maybeCommitNotAdd) <*> (not <$> maybeAddNotCommit))



