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
export PATH=$PATH:/usr/local/texlive/2012/bin/x86_64-darwin

# Cabal(Haskell)用設定
export PATH=$PATH:$HOME/.cabal/bin

# phpenv用設定
export PATH=$HOME/.phpenv/bin:$PATH

# rbenv用設定
[[ -d ~/.rbenv  ]] && \
  export PATH=${HOME}/.rbenv/bin:${PATH} && \
  eval "$(rbenv init -)"

# JAVA用PATH指定(jenv使用)
## 
## $brew install jenv
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"

# NODEBREW用PATH指定
export PATH=$HOME/.nodebrew/current/bin:$PATH
export NODEBREW_ROOT=$HOME/.nodebrew

# PYTHON用設定（pyenvを使用）
export PYENV_ROOT=${HOME}/.pyenv
export PYENV_ROOT="${HOME}/.pyenv"
if [  -d "${PYENV_ROOT}" ]; then
    export PATH=${PYENV_ROOT}/bin:$PATH
    eval "$(pyenv init -)"
fi

#perlbrew用環境設定
export PATH=$PATH:$HOME/perl5/perlbrew/bin/

# emacs用のPATH設定
#alias emacs='emacs-24.3'

# javascript用のPATH
alias js="java -cp ~/Library/rhino1_7R4/js.jar org.mozilla.javascript.tools.shell.Main"

# androidSDK for Mac 用のPATH設定
export PATH=$PATH:/Applications/android-sdk-macosx/platform-tools
ANDRIOD_HOME=$HOME/android-sdks

# MAMP mysql用
alias mmysql="/Applications/MAMP/Library/bin/mysql"
alias mmysqldump="/Applications/MAMP/Library/bin/mysqldump"

# Appium用
export PATH=$PATH:/usr/local/share/npm/bin

# Android用
export ANDROID_HOME=$HOME/android-sdks
export ANDROID_SDK=/Applications/adt-bundle-mac-x86_64-20140702/sdk/
alias android=$ANDROID_SDK/tools/android
alias adb=$ANDROID_SDK/platform-tools/adb
export NDK_ROOT=/Applications/adt-bundle-mac-x86_64-20140702/ndk/android-ndk-r10/

#ant用
export PATH=$PATH:/usr/local/Cellar/ant/1.9.2/bin

# haskell用
export PATH=$PATH:/Library/Haskell/bin

#php用
export PATH=/usr/local/php5/bin:$PATH
