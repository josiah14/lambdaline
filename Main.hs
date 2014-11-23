import LambdaLine.GitComm
import LambdaLine.System
import LambdaLine.LambdaLine
import LambdaLine.Shells.Zsh as Z
import LambdaLine.XTerm.Colors

main :: IO ()
main = exec [ ("Zsh", zshPrompt Z.shell) ]

zshPrompt :: ShellPromptType -> IO ()
zshPrompt = shellPrompt (fgColor slateBlue0 & bold $ " λ» ")

shellPrompt :: (ShellPromptType -> String) -> ShellPromptType -> IO ()
shellPrompt endChar = buildShellPrompt
                        [ bold & fgColor skyBlue `style` currentDirectory
                        , gitInformationSegment
                        ]
                        (fgColor red0 & bold $ " ➢ ")
                        endChar

gitStatusSegment :: ShellPromptType -> ShellPromptSegment String
gitStatusSegment =
  let unstagedSymbol = fgColor gold1 `style` gitUnstagedSymbol "✚"
      stagedSymbol   = fgColor orange `style` gitStagedSymbol "✎"
      pushSymbol     = fgColor red1 & bold `style` gitPushSymbol "↑"
  in prependSpace `style'` (unstagedSymbol <> stagedSymbol <> pushSymbol)

style' :: (String -> ShellPromptType -> String) 
            -> (ShellPromptType -> ShellPromptSegment String)
            -> ShellPromptType -> ShellPromptSegment String
style' f makeSegment promptType = (flip f $ promptType) <$> (makeSegment promptType)

gitInformationSegment :: ShellPromptType -> ShellPromptSegment String
gitInformationSegment =
  let branch = fgColor  deepSkyBlue3 & underline  & bold `style` gitCurrentBranch
      repoType = fgColor  defaultDarkGreen & bold `style` gitRepositorySymbol "±"
  in branch <> repoType <> gitStatusSegment 

