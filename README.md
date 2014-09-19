Lambdaline
========

Clone of Powerline in Haskell

To install, add the following to your .zshrc

function _update_ps1() {
  export PROMPT="$(~/LambdaLine/LambdaLine $COLUMNS)"
}

precmd() {
  _update_ps1
}

