import System.Directory
import System.Environment
import Data.Functor
import Data.Monoid
import Data.List as L
import LambdaLine.GitComm
import LambdaLine.PromptSegment
import LambdaLine.XTerm.Colors

getTerminalWidth :: IO (String)
getTerminalWidth = L.head <$> getArgs

currentDirectory :: IO (Maybe String)
currentDirectory = Just <$> getCurrentDirectory

main :: IO ()
main = buildMainPrompt
         [ bold . fgColor skyBlue <$> currentDirectory
         ,  (fgColor deepSkyBlue3 . underline . bold <$> gitCurrentBranch)
           <> (fgColor defaultDarkGreen . bold <$> gitRepositorySymbol "±")
           <> gitStatusSegment
         ]
         (fgColor red0 . bold <$> makePromptSegment " ➢ ")
         (fgColor slateBlue0 . bold <$> makePromptSegment " λ» ")

gitStatusSegment :: PromptSegment
gitStatusSegment =
  let unstagedSymbol = fgColor gold1 <$> gitUnstagedSymbol "✚"
      stagedSymbol   = fgColor orange <$> gitStagedSymbol "✎"
      pushSymbol     = fgColor red1 . bold <$> gitPushSymbol "↑"
  in prependSpace <$> unstagedSymbol <> stagedSymbol <> pushSymbol

