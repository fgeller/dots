shopt -s extglob
shopt -s histappend

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
# http://unix.stackexchange.com/a/1292
export HISTSIZE=""
export HISTCONTROL=ignoredups:erasedups
export HISTTIMEFORMAT="%y-%m-%d %H:%M:%S "
export HISTDIRECTORY="$HOME/.history/$(date -u +%Y/%m/%d)"
[[ ! -e "$HISTDIRECTORY" ]] && mkdir -p $HISTDIRECTORY
export HISTFILE="${HISTDIRECTORY}/$(date -u +%H.%M.%S)_${HOSTNAME}_$$"
export PROMPT_COMMAND="history -w;"
export PS1='$ '
export GOPATH=$HOME
export PATH=~/bin:/usr/local/go/bin:$GOPATH/bin:$GOROOT/bin:/usr/local/bin:/usr/local/sbin:/usr/texbin:$PATH
export EDITOR="emacsclient --alternate-editor="" -nw"

if [[ 'Darwin' == `uname` ]]
then
    GPG_AGENT=$(which gpg-agent)
    GPG_TTY=`tty`
    export GPG_TTY
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
    export PATH=/usr/local/opt/gnu-tar/libexec/gnubin:/usr/local/opt/coreutils/libexec/gnubin:$PATH
    export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_60.jdk/Contents/Home/
else
    export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/
    keychain --nocolor --quiet --agents ssh id_rsa movio_id_rsa
    source ~/.keychain/`uname -n`-sh
    source ~/.nvm/nvm.sh
fi

[[ -f /usr/local/etc/bash_completion ]] && . /usr/local/etc/bash_completion
[[ -f ${GPG_AGENT} ]] && . ~/.bash_gpg
[[ -f ~/dots/z.sh ]] && . ~/dots/z.sh

#
# Aliases
#
alias ...='cd ../..'
alias ..='cd ..'
alias a='grep -i'
alias cqlsh='cqlsh --no-color'
alias dl='cd ~/Downloads'
alias e="$EDITOR"
alias ed='cd ~/.emacs.d'
alias em="$EDITOR -e \"(progn (magit-status) (delete-other-windows))\""
alias g='git'
alias ga='git a'
alias gad='git ad'
alias gc='git c'
alias gca='git ca'
alias gcaq='git caq'
alias gco='git co'
alias gd='git d'
alias gdc='git dc'
alias gdw='git dw'
alias gff='git ff'
alias gg='git g'
alias gh='cd ~/src/github.com/fgeller'
alias gl='git l'
alias glg='git lg'
alias gll='git ll'
alias gp='git p'
alias gpu='git pu'
alias gpuf='git puf'
alias gr='git r'
alias gr='git r'
alias grh='git rh'
alias gri='git ri'
alias grs='git rs'
alias grt='if [ "`git rev-parse --show-cdup`" != "" ]; then cd `git rev-parse --show-cdup`; fi'
alias gs='git s'
alias gsh='git sh'
alias gu='git u'
alias hist='history'
alias jo='jsonify'
alias kc='kubectl '
alias kdev='kubectl --context=dev'
alias keu='kubectl --context=eu'
alias kus='kubectl --context=us'
alias l='less -R'
alias la='ls -hlA'
alias ll='ls -hlA'
alias lt='ls -ltrA'
alias m='more'
alias pa='ps aux | grep '
alias psa='ps aux'
alias td='tmux attach -d'

#
# Functions
#
function tmp {
    if [ $# -eq 0 ] ; then
	echo "Expects single argument to name temporary folder."
	return
    fi

    cd ~/tmp
    md "$1-`date +%Y-%m-%dT%H:%M:%S`"
}

function ydl {
    cd ~/Downloads && youtube-dl "$@"
}

function md {
    mkdir -p "$1"
    cd "$1"
    pwd
}

function thumb {
    file=$1
    convert $file -adaptive-resize 200x200 ${file%\.*}-resized.${file##*\.}
}

function gotest {
    ls *.go | entr bash -c 'go test |& tee quickfix'
}

function reload {
    . ~/.bashrc
    echo "loaded ~/.bashrc"
}

function jf {
    python -m json.tool
}

function f {
    find . -iname "*$@*"
}

function dpass {
    qrencode -o- `pass $1` | display
}

function pw {
    pass -c web/$1
}

function pm {
    pass -c movio/$1
}

function set_title {
    printf "\033k$1\033\\"
}

function mosh {
    set_title "m[$*]"
    command mosh "$@"
    set_title "bash"
}

function ssh {
    set_title "s[$*]"
    command ssh "$@"
    set_title "bash"
}

function rs {
    command rsync -avz --progress "$@"
}

function d {
    if [[ $1 =~ ^git@([^:]+):([^/]+)/(.+).git ]]
    then
	local td="$HOME/src/${BASH_REMATCH[1]}/${BASH_REMATCH[2]}"
	if [[ ! -d $td/${BASH_REMATCH[3]} ]]
	then
	    md $td
	    git clone $1
	else
	    echo Already cloned.
	fi
	cd $td/${BASH_REMATCH[3]}
    elif [[ $1 =~ ^https://www.youtube.com ]]
    then
	ydl $1
    elif [[ $1 =~ ^http:// || $1 =~ ^https:// ]]
    then
	cd $HOME/Downloads && wget $1
    else
	echo "dunno what to do with $1"
    fi
}

function rd {
    rm -rf "$@"
}

function ha {
    grep -ri "$@" $HOME/.history
}

function sum {
    while read n; do ((sum += n)); done; echo $sum;
}

