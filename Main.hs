import System.Directory
import System.Environment
import Data.List as L
import LambdaLine.GitComm
import LambdaLine.LambdaLine
import LambdaLine.Shells.Zsh
import LambdaLine.XTerm.Colors

getTerminalWidth :: IO String
getTerminalWidth = L.head <$> getArgs

currentDirectory :: PromptSegment String
currentDirectory = convertToPromptSegment $ Just <$> getCurrentDirectory

main :: IO ()
main = buildMainPrompt
         [ bold . fgColor skyBlue <$> currentDirectory
         , gitInformationSegment
         ]
         (fgColor red0 . bold $ " ➢ ")
         (fgColor slateBlue0 . bold $ " λ» ")

gitStatusSegment :: PromptSegment String
gitStatusSegment =
  let unstagedSymbol = fgColor gold1 <$> gitUnstagedSymbol "✚"
      stagedSymbol   = fgColor orange <$> gitStagedSymbol "✎"
      pushSymbol     = fgColor red1 . bold <$> gitPushSymbol "↑"
  in prependSpace <$> unstagedSymbol <> stagedSymbol <> pushSymbol

gitInformationSegment :: PromptSegment String
gitInformationSegment =
  let branch = fgColor deepSkyBlue3 . underline . bold <$> gitCurrentBranch
      repoType = fgColor defaultDarkGreen . bold <$> gitRepositorySymbol "±"
  in branch <> repoType <> gitStatusSegment

