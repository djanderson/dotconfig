#!/bin/bash

dotfiles=~/dotfiles
oldfiles=~/dotfiles_old
files="bashrc emacs.d"

mkdir -p $oldfiles
cd $dotfiles

for file in $files; do
    mv -f ~/.$file $oldfiles
    echo "Creating symlink to $file in ~/.${file}."
    ln -Tfs $dotfiles/$file ~/.$file
done
