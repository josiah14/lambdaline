Lambdaline
========

Clone of Powerline in Haskell

*ZSH only is implemented/supported as of yet*

To install, add the following to your .zshrc

function _update_ps1() {
  export PROMPT="$($LAMBDALINE_HOME/Main $COLUMNS)"
}

precmd() {
  _update_ps1
}


*Note* This currently only supports editing $PS1/$PROMPT to update the status line.  This will eventually be expanded to include, also, the $RPROMPT as well, and also multi-line prompts, in that order.

For multi-line prompts, the eventual goal is to eliminate segments of the prompt as the window shrinks in order of importance to the user (as specified via some sort of configuration step, or according to the defaults).

Future Support, in order of importance:
- SVN support
- Mercurial support
- Bash
- TMux status line
- Vim Status line
- Bazaar support
- CVS support

