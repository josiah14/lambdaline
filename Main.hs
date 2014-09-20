import System.Directory
import System.Environment
import Control.Monad
import Data.List as L
import Data.Maybe
-- import LambdaLine.Util
import LambdaLine.GitComm
import LambdaLine.Segment

getTerminalWidth :: IO (String)
getTerminalWidth = liftM L.head getArgs

main :: IO ()
main = buildMainPrompt
         [ liftM Just getCurrentDirectory >>= space
         , (gitStatusSymbols "✚" "✎" "↑" >>= space) >+< gitCurrentBranch >+< (gitRepositorySymbol "±" >>= space)
         ]
         "λ "

