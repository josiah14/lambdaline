import System.Directory
import System.Process
import Data.Text as T
import Control.Monad
import System.Exit

type ProcessResponse = IO (ExitCode, String, String)
type BranchName = IO (Maybe String)

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
                     ExitFailure _    -> return Nothing
                     where trim = unpack . strip . pack

