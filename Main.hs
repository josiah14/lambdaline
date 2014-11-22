{-# LANGUAGE FlexibleContexts #-}

import System.Directory
import System.Environment
import LambdaLine.GitComm
import LambdaLine.LambdaLine
import LambdaLine.Shells.Zsh as Z
import LambdaLine.Shells.ShellPromptSegment
import LambdaLine.XTerm.Colors

currentDirectory :: ShellPromptSegment String
currentDirectory = convertToPromptSegment $ Just <$> getCurrentDirectory

getPromptType :: IO String
getPromptType = head <$> getArgs

exec :: [(String, IO())] -> IO ()
exec prompts = getPromptType >>= (\promptType -> snd $ head $ dropWhile (\testVal -> promptType /= fst testVal) prompts)


main :: IO ()
main = exec [ ("Zsh", zshPrompt) ]

zshPrompt :: IO ()
zshPrompt = buildPrompt
              [ bold zshStyler . fgColor zshStyler skyBlue <$> currentDirectory
              , gitInformationSegment zshStyler
              ]
              (fgColor zshStyler red0 . bold zshStyler $ " ➢ ")
              (fgColor zshStyler slateBlue0 . bold zshStyler $ " λ» ")


gitStatusSegment :: ShellPromptStyler -> ShellPromptSegment String
gitStatusSegment styler =
  let unstagedSymbol = fgColor styler gold1 <$> gitUnstagedSymbol "✚"
      stagedSymbol   = fgColor styler orange <$> gitStagedSymbol "✎"
      pushSymbol     = fgColor styler red1 . bold styler <$> gitPushSymbol "↑"
  in prependSpace styler <$> unstagedSymbol <> stagedSymbol <> pushSymbol

gitInformationSegment :: ShellPromptStyler -> ShellPromptSegment String
gitInformationSegment styler =
  let branch = fgColor styler deepSkyBlue3 . underline styler . bold styler <$> gitCurrentBranch
      repoType = fgColor styler defaultDarkGreen . bold styler <$> gitRepositorySymbol "±"
  in branch <> repoType <> gitStatusSegment styler

