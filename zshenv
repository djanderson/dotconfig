export XDG_CONFIG_HOME="$HOME/.config"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export PATH="$PATH:$XDG_CONFIG_HOME/emacs/bin"

eval "$(/opt/homebrew/bin/brew shellenv)"
. "$HOME/.cargo/env"
