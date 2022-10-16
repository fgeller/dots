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
export PATH="~/.node_modules/bin:$PATH"
export PATH="/home/fgeller/.yarn/bin:$PATH"

cdpath=(~)
for pth in ~/src/github.com/* ; do cdpath+=($pth) ; done
export cdpath

export PS1='%F{244}%1~%f %(?.%F{green}%#%f.%F{red}%#%f) '
export RPROMPT='' # ensure empty right side

# source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

EDITOR='emacsclient'

bindkey '^[[1;5D' backward-word
bindkey '^[[1;5C' forward-word
bindkey '^H' backward-kill-word

alias grt='if [ "`git rev-parse --show-cdup`" != "" ]; then cd `git rev-parse --show-cdup`; fi'
alias gs="git status -s -b"
alias ll="ls -la"
alias reload="source ~/.zshrc"
alias g='git'
alias gll='git ll'
alias pc='pass show -c'
alias k='kubectl'
alias j='just'

if [ Darwin = `uname` ]; then
   export NVM_DIR="$HOME/.nvm"
   [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
   source "/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc"
   source "/opt/homebrew/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc"
   export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
   export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
   export PATH="/opt/homebrew/opt/findutils/libexec/gnubin:$PATH"
   export PATH="/opt/homebrew/opt/make/libexec/gnubin:$PATH"
   export PATH="/opt/homebrew/bin:$PATH"
else
    export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
fi

function maybeJSON() {
    tee >(grep -v "^\{") | grep "^\{" | jq -c .

    #docker logs --follow 3a8ac5b98381 |& grep "^\{" | while read l ; do echo $l ; done | jq
}

eval "$(direnv hook zsh)"
