# Lines configured by zsh-newuser-install
setopt autocd extendedglob nomatch
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/fgeller/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

export HISTFILE=~/.history/.histfile
export HISTSIZE=1000000
export SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups
setopt hist_ignore_space # ignore entries that start with at least one space

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

export GOPATH=$HOME
export GO111MODULE=on
export GOPROXY=https://proxy.golang.org

export PATH=/usr/local/go/bin:$GOPATH/bin:$GOROOT/bin:$PATH
export PATH=~/bin:/usr/local/bin:/usr/local/sbin:/usr/texbin:~/.local/bin:$PATH
export PATH="~/.node_modules/bin:$PATH"

cdpath=(~ ~/src/github.com/fgeller ~/src/gitlab.com/refurbed)

PS1='%F{244}%1~%f %(?.%#.%F{red}%#%f) '

# source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

EDITOR='emacsclient -a /bin/nano'

alias grt='if [ "`git rev-parse --show-cdup`" != "" ]; then cd `git rev-parse --show-cdup`; fi'
alias gs="git status -s -b"
alias ll="ls -la"
alias reload="source ~/.zshrc"
alias g='git'
alias pc='pass show -c'
alias k='kubectl'

export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

function maybeJSON() {
    tee >(grep -v "^\{") | grep "^\{" | jq -c .

    #docker logs --follow 3a8ac5b98381 |& grep "^\{" | while read l ; do echo $l ; done | jq
}

alias goose-local='goose -dir ~/src/gitlab.com/refurbed/platform/misc/migrations/ postgres "dbname=refurbed password=refurbed user=refurbed sslmode=disable"'
