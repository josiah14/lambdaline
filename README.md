Lambdaline
========

Clone of Powerline in Haskell
- Though the implementation relies heavily on Monads, Functors, Applicative Functors, and the like, the high level API was designed so that one does not need to know what these things are to build a decent configuration.  The intention is that the example Main.hs file should be intuitive enough for most programmers to figure out the basic DSL language for building prompts and status lines with this program.

*ZSH only is implemented/supported as of yet*

#Build from Source
- git clone this repository, then navigate to it in the command line
- run 'ghc Main.hs' in the top level directory of the lambdaline source.

#Installation

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

