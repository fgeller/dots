# Lines configured by zsh-newuser-install
setopt autocd extendedglob nomatch
setopt interactive_comments
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/Users/fgeller/.zshrc'

# Manually added
fpath=(/opt/homebrew/share/zsh/site-functions $fpath)

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
export GOGC=2000

if [ Darwin = `uname` ]; then
	eval "$(/opt/homebrew/bin/brew shellenv)"
fi

export PATH=/usr/local/go/bin:$GOPATH/bin:$GOROOT/bin:$PATH
export PATH=/usr/local/bin:/usr/local/sbin:/usr/texbin:~/.local/bin:$PATH
export PATH=$PATH:/usr/sbin
export PATH="~/.node_modules/bin:$PATH"
export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/libpq/bin:$PATH"
export PATH="/opt/homebrew/opt/pnpm@8/bin:$PATH"
export PATH="~/bin:/opt/homebrew/bin:$PATH"

export PASSWORD_STORE_ENABLE_EXTENSIONS=true

export TERM=xterm-24bit
export TERMINFO=~/.terminfo
export GPG_TTY=$(tty) # needed when using ssh

# https://github.com/zsh-users/zsh-autosuggestions/
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#6d7f82"

source <(jj util completion zsh)

cdpath=(~)
for pth in ~/src/github.com/* ; do cdpath+=($pth) ; done
export cdpath

export PS1='%F{244}%1~%f %(?.%F{green}%#%f.%F{red}%#%f) '
export RPROMPT='' # ensure empty right side
export EDITOR='emacsclient -nw'
export PAGER='emacs-pager'

export PRE_COMMIT_OPT_OUT=true

export DFT_SYNTAX_HIGHLIGHT=off
export DFT_BACKGROUND=light

autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

bindkey '^[[1;5D' backward-word
bindkey '^[[1;5C' forward-word
bindkey '^H' backward-kill-word

alias e="emacsclient "
alias eq="emacs --debug-init -q -nw"
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
alias gpf="git push --force-with-lease"
alias gpr="git push && gh pr create"
alias gsa="git stash apply stash@{0}"
alias gu="git fetch origin"
alias gss="gsync"
alias gm="git checkout -q main && gs"
alias gfu="git_fixup_last_n"

alias jjl="jj log -r ::@ --limit 10"

alias z="zellij"

function pc() {
	pass show "$1" | head -1  | tr -d '\n' | pbcopy
}

function got() {
	go test -test.v=true -run "$@" ./...
}

function gch() {
	git checkout "$(git branch --all | rg -v remotes/origin | sk --query=$@ | tr -d '[:space:]')"
}

function gchh() {
	git checkout "$(git branch --all | sk --query=$@ | tr -d '[:space:]')"
}

gsync() {
	local current_branch
	current_branch=$(git symbolic-ref --short HEAD)

	if [[ $current_branch != "main" ]]; then
		echo "> switch to main"
		git checkout --quiet main
	fi

	echo "> git fetch --prune"
	git fetch --prune --quiet

	echo "> fast-forward main"
	git merge --ff-only --quiet origin/main

	local gone_branches
	gone_branches=$(git for-each-ref --format '%(refname) %(upstream:track)' refs/heads | rg '\[gone\]' | sed -E 's:refs/heads/::g' | cut -d' ' -f1)
	gone_branches=(${gone_branches:#}) # filter empty strings
	if [[ -n $gone_branches ]] ; then
		for branch in "${(@f)gone_branches}"; do
			read -q "REPLY?> delete branch '$branch' (remote gone)? (y/n) "
			echo
			if [[ $REPLY =~ ^[Yy]$ ]]; then
				echo "> deleting $branch"
				git branch -D "$branch"
			fi
		done
	fi
	
	local all_branches
	all_branches=$(git for-each-ref refs/heads/ "--format=%(refname:short)" | grep -vE "(main|master)")

	for branch in "${(@f)all_branches}"; do
		echo -n "> rebasing $branch "
		git checkout -q $branch
		git rebase origin/main &>/dev/null
		if [ $? -ne 0 ]; then
			echo "❌"
			git rebase --abort
		else
			echo "✅"
		fi
	done

	echo "> back to branch $current_branch"
	git checkout -q $current_branch
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

function git_fixup_last_n() {
  if [[ -z "$1" || ! "$1" =~ ^[0-9]+$ ]]; then
    echo "required number of commits to silently squash"
    return 1
  fi
  git reset --soft HEAD~$1 && git commit --amend --no-edit
}


# jj push: set bookmark, git push, new
function jjp() {
  if [[ $# -ne 2 ]]; then
    echo "Usage: jjp <git-branch> <jj-revision>"
    return 1
  fi

  local git_branch=$1
  local jj_revision=$2

  jj bookmark set -r $jj_revision $git_branch
  jj git push --allow-new -b $git_branch
  jj new
}

# Enable completion for the first argument (Git branches)
_jjp_complete_git_branches() {
  compadd -- $(git branch --format='%(refname:short)')
}

compdef _jjp_complete_git_branches jjp
