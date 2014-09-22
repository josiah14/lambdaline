import System.Directory
import System.Environment
import Control.Monad
import Data.List as L
import LambdaLine.GitComm
import LambdaLine.PromptSegment
import LambdaLine.XTerm.Colors

getTerminalWidth :: IO (String)
getTerminalWidth = liftM L.head getArgs

main :: IO ()
main = buildMainPrompt
         [ liftM Just getCurrentDirectory >>= bold >>= fgColor grey0 >>= bgColor defaultWhite
         , (gitCurrentBranch >>= fgColor deepSkyBlue3 >>= underline >>= bold) 
           >+< (gitRepositorySymbol "±" >>= fgColor defaultDarkGreen >>= bold >>= space)
           >+< ( 
                 ( 
                   (gitUnstagedSymbol "✚" >>= fgColor gold1)
                   >+< (gitStagedSymbol "✎" >>= fgColor orange)
                   >+< (gitPushSymbol "↑" >>= fgColor red1 >>= bold)
                 )
               )
         ]
         (makePromptSegment " ➢ " >>= fgColor slateBlue0 >>= bold)
         (makePromptSegment " λ» " >>= fgColor slateBlue0 >>= bold)

