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
         , ( 
             ( 
               (gitUnstagedSymbol "✚" >>= fgColor gold1)
               >+< (gitStagedSymbol "✎" >>= fgColor darkOrange1)
               >+< (gitPushSymbol "↑" >>= fgColor red1)
             ) >>= space
           ) 
           >+< (gitCurrentBranch >>= fgColor deepSkyBlue3)
           >+< (gitRepositorySymbol "±" >>= fgColor defaultDarkGreen >>= space)
         ]
         (separator "➢ " >>= fgColor slateBlue0)
         (promptSymbol "λ " >>= fgColor slateBlue0)

