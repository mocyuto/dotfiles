#!/bin/sh

files=(".gitconfig" ".tmux.conf" ".zshrc" ".zshenv" ".zinitrc")
for f in "${files[@]}"
do
  current_dir=`pwd`
  ln -s $current_dir/$f $HOME/$f
done
