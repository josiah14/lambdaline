import System.Directory
import System.Environment
import Control.Monad
import Data.List as L
import LambdaLine.GitComm
import LambdaLine.Segment
import LambdaLine.XTerm.Colors

getTerminalWidth :: IO (String)
getTerminalWidth = liftM L.head getArgs

main :: IO ()
main = buildMainPrompt
         [ liftM Just getCurrentDirectory >>= space
         , (((gitUnstagedSymbol "✚" >>= color gold1)
           >+< (gitStagedSymbol "✎" >>= color darkOrange1)
           >+< (gitPushSymbol "↑" >>= color red1)) >>= space)
           >+< gitCurrentBranch
           >+< (gitRepositorySymbol "±" >>= space)
         ]
         "➢ "
         "λ "

