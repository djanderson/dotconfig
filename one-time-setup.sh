#!/usr/bin/env bash

CONFIG_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ln -sv "$CONFIG_DIR"/zshenv $HOME/.zshenv

git clone --depth 1 https://github.com/doomemacs/doomemacs "$CONFIG_DIR"/emacs
