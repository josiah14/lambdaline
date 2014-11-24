import LambdaLine.GitComm
import LambdaLine.System
import LambdaLine.LambdaLine
import LambdaLine.Shells.Zsh as Z
import LambdaLine.XTerm.Colors

main :: IO ()
main = exec [ ("Zsh", zshPrompt Z.shell) ]

zshPrompt :: ShellType -> IO ()
zshPrompt = shellPrompt (fgColor slateBlue0 & bold $ " λ» ")

-- Takes the ending char to the prompt as the first argument and applies it
-- as the second to last argument of buildShellPrompt to return a function
-- that recieves the shell type as input
shellPrompt :: (ShellType -> String) -> ShellType -> IO ()
shellPrompt = buildShellPrompt
                        [ bold & fgColor skyBlue `style` mkShellSegment currentDirectory
                        , gitInformationSegment
                        ]
                        (fgColor red0 & bold $ " ➢ ")

gitStatusSegment :: ShellType -> ShellSegment String
gitStatusSegment =
  let unstagedSymbol = fgColor gold1 `style` mkShellSegment (gitUnstagedSymbol "✚")
      stagedSymbol   = fgColor orange `style` mkShellSegment (gitStagedSymbol "✎")
      pushSymbol     = fgColor red1 & bold `style` mkShellSegment (gitPushSymbol "↑")
  in prependSpace `style` unstagedSymbol <> stagedSymbol <> pushSymbol

gitInformationSegment :: ShellType -> ShellSegment String
gitInformationSegment =
  let branch = fgColor deepSkyBlue3 & underline & bold `style` mkShellSegment gitCurrentBranch
      repoType = fgColor defaultDarkGreen & bold `style` (mkShellSegment $ gitRepositorySymbol "±")
  in branch <> repoType <> gitStatusSegment

