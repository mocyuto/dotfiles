#!/bin/sh

files=(".gitconfig" ".tmux.conf" ".zshrc" ".zshenv" ".zplugrc")
for f in "${files[@]}"
do
  current_dir=`pwd`
  ln -s $current_dir/$f $HOME/$f
done

