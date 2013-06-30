export LANG=ja_JP.UTF-8

# カラー設定
export LSCOLORS=ExFxCxdxBxegedabagacad
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# gnuplot等のグラフ出力先をX11に指定
alias gnuplot='/Applications/gnuplot.app/gnuplot'
export GNUTERM=X11

# Homebrew用PATH指定
export PATH=/usr/local/bin:/usr/local/sbin:/usr/local/share:/usr/local/share/python:$PATH

# TeXLive用PATH指定
export PATH=$PATH:/usr/local/texlive/2012/bin/x86_64-darwin

# Cabal(Haskell)用設定
export PATH=$PATH:$HOME/.cabal/bin

# rbenv,phpenv用設定
export PATH=$HOME/.phpenv/bin:$HOME/.rbenv/bin:$HOME/.rbenv/shims:$PATH

# JAVA用PATH指定
# export JAVA_HOME=/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home
export JAVA_HOME=/Library/Java/Home
alias javac="javac -J-Dfile.encoding=UTF-8"
alias java="java -Dfile.encoding=UTF-8"

# NODEBREW用PATH指定
export PATH=$HOME/.nodebrew/current/bin:$PATH
export NODEBREW_ROOT=$HOME/.nodebrew

# PYTHON用設定（pythonbrewを使用）
export PATH=/usr/local/bin:/usr/local/share/python:$PATH:/Applications/eclipse/android-sdk-mac_x86/tools
#export PYTHONPATH=$HOME/.pythonbrew/current:$(brew --prefix)/lib/python2.7/site-packages:$PYTHONPATH
#export WORKON_HOME=$HOME/.virtualenvs

# perlbrew用環境設定
export PATH=$PATH:$HOME/perl5/perlbrew/bin/

# emacs用のPATH設定
#alias emacs='emacs-24.3'

# javascript用のPATH
alias js="java -cp ~/Library/rhino1_7R4/js.jar org.mozilla.javascript.tools.shell.Main"

# virtualenv用
export PATH=$PATH:/opt/local/bin
source /usr/local/share/python/virtualenvwrapper.sh
export WORKON_HOME=$HOME/.virtualenvs
export PIP_RESPECT_VIRTUALENV=true

# androidSDK for Mac 用のPATH設定
export PATH=$PATH:/Applications/android-sdk-macosx/platform-tools
