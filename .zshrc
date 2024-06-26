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

export HISTSIZE=10000000
export SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups
setopt hist_ignore_space # ignore entries that start with at least one space

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

export GOPATH=$HOME
export GO111MODULE=on
export GOPROXY=https://proxy.golang.org

if [ Darwin = `uname` ]; then
	eval "$(/opt/homebrew/bin/brew shellenv)"
	source "/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc"
fi

export PATH=/usr/local/go/bin:$GOPATH/bin:$GOROOT/bin:$PATH
export PATH=/usr/local/bin:/usr/local/sbin:/usr/texbin:~/.local/bin:$PATH
export PATH=$PATH:/usr/sbin
export PATH="~/.node_modules/bin:$PATH"
export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/libpq/bin:$PATH"
export PATH="~/bin:$PATH"

export PASSWORD_STORE_ENABLE_EXTENSIONS=true

export TERM=xterm
export GPG_TTY=$(tty) # needed when using ssh

# https://github.com/zsh-users/zsh-autosuggestions/
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#6d7f82"


cdpath=(~)
for pth in ~/src/github.com/* ; do cdpath+=($pth) ; done
export cdpath

export PS1='%F{244}%1~%f %(?.%F{green}%#%f.%F{red}%#%f) '
export RPROMPT='' # ensure empty right side
export EDITOR='hx'
export PRE_COMMIT_OPT_OUT=true

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

alias gtm='gt modify --no-verify -c'

alias ga="git add"
alias gbb="git checkout -b"
alias gbl="git blame"
alias gbs="git branch --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(contents:subject) %(color:green)(%(committerdate:relative))' --sort=committerdate"
alias gdm="git diff origin/main...HEAD"
alias gds="git diff --name-only"
alias gff="git fetch origin && git merge --ff-only origin/main"
alias gmm="git fetch origin && git merge origin/main"
alias gp="git push"
alias gpr="git push && gh pr create"
alias gsa="git stash apply stash@{0}"
alias gu="git fetch origin"
alias lg="lazygit"

function pc() {
	pass show "$1" | head -1  | tr -d '\n' | pbcopy
}

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
	local message
	message="$*"

	if [[ -n "$message" ]]; then
		git commit -n -m "$message"
	else
		git commit -n
	fi
}

function loc_dirs() {
  local lang=$1
  if [[ -z $1 ]]; then
    echo "defaulting to Go."
	lang=Go
  fi

  find . -mindepth 1 -maxdepth 1 -type d | while read -r d; do
    tokei -t "$lang" "$d" | rg "$lang" | awk -v dir=" $d" '{print $4 dir}'
  done | sort -n
}
