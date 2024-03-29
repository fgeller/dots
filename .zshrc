# Lines configured by zsh-newuser-install
setopt autocd extendedglob nomatch
setopt interactive_comments
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/fgeller/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export HISTFILE=~/.history/.histfile
export HISTSIZE=10000000
export SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups
setopt hist_ignore_space # ignore entries that start with at least one space
setopt share_history

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

export GOPATH=$HOME
export GO111MODULE=on
export GOPROXY=https://proxy.golang.org

export PATH=/usr/local/go/bin:$GOPATH/bin:$GOROOT/bin:$PATH
export PATH=~/bin:/usr/local/bin:/usr/local/sbin:/usr/texbin:~/.local/bin:$PATH
export PATH=$PATH:/usr/sbin
export PATH="~/.node_modules/bin:$PATH"

export DFT_SYNTAX_HIGHLIGHT=off
export DFT_BACKGROUND=light
export DFT_TAB_WIDTH=4
export PASSWORD_STORE_ENABLE_EXTENSIONS=true

cdpath=(~)
for pth in ~/src/github.com/* ; do cdpath+=($pth) ; done
export cdpath

export PS1='%F{244}%1~%f %(?.%F{green}%#%f.%F{red}%#%f) '
export RPROMPT='' # ensure empty right side

export EDITOR='emacsclient -nw'

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

bindkey '^[[1;5D' backward-word
bindkey '^[[1;5C' forward-word
bindkey '^H' backward-kill-word

alias grt='if [ "`git rev-parse --show-cdup`" != "" ]; then cd `git rev-parse --show-cdup`; fi'
alias gs="git status -s -b"
alias gd="git diff"
alias gdc="git diff --cached"
alias ll="ls -la"
alias reload="source ~/.zshrc"
alias g='git'
alias gll='git ll'
alias pc='pass show -c'
alias k='kubectl'
alias j='just'
alias e='emacsclient -nw'

alias ga="git add"
alias gb="git checkout -b"
alias gbs="git branch --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(contents:subject) %(color:green)(%(committerdate:relative))' --sort=committerdate"
alias gdm="git diff origin/main...HEAD"
alias gds="git diff --name-only"
alias gff="git fetch origin && git merge --ff-only origin/main"
alias gmm="git fetch origin && git merge origin/main"
alias gp="git push"
alias gpr="git push && gh pr create"
alias gsa="git stash apply stash@{0}"
alias gu="git fetch origin"

alias jjd="jj diff"

function gch() {
	git checkout "$(git branch --all | fzf --query=$@ | tr -d '[:space:]')"
}

function jjpr() {
  local selected_branch
  selected_branch=$(jj branch list | cut -d: -f1 | fzf --query="$@" | tr -d '[:space:]')

  if [[ -n "$selected_branch" ]]; then
    jj git push --branch "$selected_branch"
    gh pr create -H "$selected_branch"
  else
    echo "No branch selected."
  fi
}

function jjp() {
  local selected_branch
  selected_branch=$(jj branch list | cut -d: -f1 | fzf --query="$@" | tr -d '[:space:]')

  if [[ -n "$selected_branch" ]]; then
    jj git push --branch "$selected_branch"
  else
    echo "No branch selected."
  fi
}

function gc() {
	git commit -m "$*"
}

function gss() {
	git add .
	git commit -m chip
}

function maybeJSON() {
	tee >(grep -v "^\{") | grep "^\{" | jq -c .
}

if [ Darwin = `uname` ]; then
	eval "$(/opt/homebrew/bin/brew shellenv)"
	source "/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc"
	export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
	export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
	export PATH="/opt/homebrew/opt/libpq/bin:$PATH"
fi

export TERM=xterm-24bit
# needed when using ssh
export GPG_TTY=$(tty)
