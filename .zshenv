export LANG=ja_JP.UTF-8

# カラー設定
export LSCOLORS=ExFxCxdxBxegedabagacad
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# lessカラー設定
export LESS=' -R'
export LESSOPEN="| /usr/local/bin/src-hilite-lesspipe.sh %s"

# gnuplot等のグラフ出力先をX11に指定
alias gnuplot='/Applications/gnuplot.app/gnuplot'
export GNUTERM=X11

# Homebrew用PATH指定
export PATH=/usr/local/bin:/usr/local/sbin:/usr/local/share:/usr/local/share/python:$PATH

# TeXLive用PATH指定
# export PATH=$PATH:/usr/local/texlive/2012/bin/x86_64-darwin

# Cabal(Haskell)用設定
# export PATH=$PATH:$HOME/.cabal/bin
# export PATH=$PATH:/Library/Haskell/bin

# phpenv用設定
# export PATH=$HOME/.phpenv/bin:$PATH

# rbenv用設定
[[ -d ~/.rbenv  ]] && \
  export PATH=${HOME}/.rbenv/bin:${PATH} && \
  eval "$(rbenv init -)"

# JAVA用PATH指定(jenv使用)
## $brew install jenv
if [ -d ~/.jenv ]; then
    export PATH="$HOME/.jenv/bin:$PATH"
    eval "$(jenv init -)"
fi

# NODEBREW用PATH指定
export PATH=$HOME/.nodebrew/current/bin:$PATH
export NODEBREW_ROOT=$HOME/.nodebrew

# PYTHON用設定（pyenvを使用）
export PYENV_ROOT=${HOME}/.pyenv
if [  -d "${PYENV_ROOT}" ]; then
    export PATH=${PYENV_ROOT}/bin:$PATH
    eval "$(pyenv init -)"
fi

# poetryの設定
export PATH="$HOME/.poetry/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

#perlbrew用環境設定
# export PATH=$PATH:$HOME/perl5/perlbrew/bin/

# emacs用のPATH設定
# alias emacs='emacs-24.3'

# javascript用のPATH
alias js="java -cp ~/Library/rhino1_7R4/js.jar org.mozilla.javascript.tools.shell.Main"

# Android SDK for Mac 用のPATH設定
export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
export ANDROID_SDK=/Applications/adt-bundle-mac-x86_64-20140702/sdk/
alias android=$ANDROID_SDK/tools/android
alias adb=$ANDROID_SDK/platform-tools/adb
export NDK_ROOT=/Applications/adt-bundle-mac-x86_64-20140702/ndk/android-ndk-r10/

#ant用
export PATH=$PATH:/usr/local/Cellar/ant/1.9.2/bin

#php用
export PATH=/usr/local/php5/bin:$PATH

# Rust用
export PATH="$HOME/.cargo/bin:$PATH"
source "$HOME/.cargo/env"

# Go
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
# goenv
if [ -d ~/.goenv ]; then
    export PATH="$HOME/.goenv/bin:$PATH"
    eval "$(goenv init -)"
fi

# MySQL Client by brew
export PATH="/usr/local/opt/mysql-client/bin:$PATH"

# for krew
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/yuto.suzuki/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/yuto.suzuki/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/yuto.suzuki/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/yuto.suzuki/google-cloud-sdk/completion.zsh.inc'; fi

# aqua
export PATH="${AQUA_ROOT_DIR:-${XDG_DATA_HOME:-$HOME/.local/share}/aquaproj-aqua}/bin:$PATH"
