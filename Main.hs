import System.Directory
import System.Environment
import Control.Monad
import Data.List as L
import LambdaLine.GitComm
import LambdaLine.Segment

getTerminalWidth :: IO (String)
getTerminalWidth = liftM L.head getArgs

main :: IO ()
main = buildMainPrompt
         [ liftM Just getCurrentDirectory >>= space
         , (gitStatusSymbols "✚" "✎" "↑" >>= color red >>= space) >+< gitCurrentBranch >+< (gitRepositorySymbol "±" >>= space)
         ]
         "λ "

