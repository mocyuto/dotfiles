# Created by newuser for 4.3.10
# users generic .zshrc file for zsh(1)

## Environment variable configuration
#
# LANG
#
export LANG=ja_JP.UTF-8

## Default shell configuration
#
# set prompt

# VCSの情報を取得するzshの便利関数 vcs_infoを使う
autoload -Uz vcs_info

## 表示フォーマットの指定
# %b ブランチ情報
# %a アクション名(mergeなど)
zstyle ':vcs_info:*' formats '[%b]'
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
    if [ ! -z $TMUX ]; then
      tmux refresh-client -S
    fi
}

# バージョン管理されているディレクトリにいれば表示，そうでなければ非表示

autoload colors
colors
# ディレクトリとGitを表示
PROMPT1=" %{${fg[green]}%}%B%/%b%{${reset_color}%} %1(v|%F{green}%1v%f|)"
PROMPT2="[%n@%m]$ "

# 右側にk8s context を表示する
PROMPT_K8S='%{$fg[cyan]%}($ZSH_KUBECTL_PROMPT)%{$reset_color%}'

PROMPT="$PROMPT1 $PROMPT_K8S
$PROMPT2"
# 右側に時間を表示する
RPROMPT="%T"
# 右側まで入力がきたら時間を消す""
setopt transient_rprompt

##############################
########### 補完 #############
##############################

## Completion configuration
if type brew &>/dev/null; then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi
autoload -Uz compinit && compinit

# auto change directory
setopt auto_cd
function chpwd() { ls -G }

# auto directory pushd that you can get dirs list by cd -[tab]
setopt auto_pushd

#タブキー連打で補完候補を順に表示
setopt auto_menu

#補完候補が複数ある時に一覧表示
setopt auto_list

# カーソルの位置に補なうことで単語を完成させようとする。
setopt complete_in_word

# command correct edition before each completion attempt
setopt correct

# compacked complete list display
#
setopt list_packed

# no remove postfix slash of command line
#
setopt noautoremoveslash

# no beep sound when complete list displayed
#
setopt nolistbeep

#補完候補リストの日本語を正しく表示
setopt print_eight_bit

#補完の時に大文字小文字を区別しない（ただし大文字を打ったときは小文字にしない）
# r:|[._-]=*: 「.」「_」「-」の前にワイルドカード「*」があるものとして補完する。
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z} r:|[.]=*'
# 補完候補を ←↓↑選択 (候補が色分け表示される)
zstyle ':completion:*:default' menu select=1
# completions get colors
zstyle ':completion:*' list-colors di=35 fi=0
# カレントディレクトリに候補がない場合のみ cdpath 上のディレクトリを候補
zstyle ':completion:*:cd:*' tag-order local-directories path-directories
# 特定のファイルは補完対象から除外する
zstyle ':completion:*' ignored-patterns '*.pyc'

## kubectl completion
source <(kubectl completion zsh)

#############################

## Keybind configuration
#
# emacs like keybind (e.x. Ctrl-a goes to head of a line and Ctrl-e goes
#   to end of it)
#
bindkey -e

# historical backward/forward search with linehead string binded to ^P/^N
#
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
bindkey "\\ep" history-beginning-search-backward-end
bindkey "\\en" history-beginning-search-forward-end


## Command history configuration
#
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data
setopt append_history       # 履歴を追加（毎回.zsh_historyを作らない）
setopt rm_star_wait         #rm * を実行する前に確認する

## Prediction configuration
#
#autoload predict-on
#predict-off

## 実行時間が３秒以上の場合表示
REPORTTIME=3

## Alias configuration
#
# expand aliases before completing
#`
#####################

setopt completealiases     # aliased ls needs if file/dir completions work
#setopt extendedglob

alias where="command -v"
alias j="jobs -l"

case "${OSTYPE}" in
darwin*)
alias ls="ls -G"
alias ll="ls -lG"
alias la="ls -laG"
;;
linux*)
alias ls='ls --color'
alias ll='ls -l --color'
alias la='ls -la --color'
;;
esac

#alias la="ls -a"
alias du="du -h"
alias df="df -h"

alias su="su -l"
alias sl="ls"
#alias ls="ls --color=auto --hide='*.pyc'"

alias sed="gsed"

### kube
alias k="kubectl"

### IntelliJ
alias ij='open -b com.jetbrains.intellij'

## eval direnv
(( $+commands[direnv] )) && eval "$(direnv hook zsh)"

### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

source ~/.zinitrc
### End of Zinit's installer chunk

### NVM setting
export NVM_DIR="$HOME/.nvm"
export NVM_COMPLETION=true
export NVM_LAZY_LOAD=true

