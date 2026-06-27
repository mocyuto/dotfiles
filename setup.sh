#!/bin/bash

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

files=(".gitconfig" ".tmux.conf" ".zshrc" ".zshenv" ".zinitrc")
for f in "${files[@]}"; do
  src="$script_dir/$f"
  dst="$HOME/$f"
  if [[ -e "$dst" || -L "$dst" ]]; then
    backup="${dst}.bak.$(date +%s)"
    echo "Backing up existing $dst -> $backup"
    mv "$dst" "$backup"
  fi
  ln -s "$src" "$dst"
done

# Install mise
curl https://mise.run | sh
export PATH="$HOME/.local/bin:$PATH"
eval "$($HOME/.local/bin/mise activate bash)"

mise trust
mise install

rulesync install
rulesync generate
