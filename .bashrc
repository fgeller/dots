shopt -s extglob
shopt -s histappend

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

#
# Keep all the histories!
#
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
export GO111MODULE=on

export PATH=~/src/github.com/flutter/flutter/bin:$PATH
export PATH=/usr/local/opt/findutils/libexec/gnubin:$PATH
export PATH=/usr/local/opt/gnu-tar/libexec/gnubin:$PATH
export PATH=/usr/local/go/bin:$GOPATH/bin:$GOROOT/bin:$PATH
export PATH=~/bin:/usr/local/bin:/usr/local/sbin:/usr/texbin:~/.local/bin:$PATH
export PATH="~/.node_modules/bin:$PATH"

export CDPATH=".:~:~/src/github.com/fgeller"

export TERM=xterm-24bit
export EDITOR="emacsclient -nw -a /usr/bin/nano"

export npm_config_prefix=~/.node_modules

# makefile target auto-completion
# https://zgadzaj.com/development/makefile/bash-auto-completion-for-makefile-targets-in-macos
complete -W "\`grep -oE '^[a-zA-Z0-9_.-]+:([^=]|$)' makefile | sed 's/[^a-zA-Z0-9_.-]*$//'\`" make

if [[ 'Darwin' == `uname` ]]
then
    GPG_AGENT=$(which gpg-agent)
    GPG_TTY=`tty`
    export GPG_TTY
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
    export PATH=/usr/local/opt/gnu-tar/libexec/gnubin:/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/openjdk/bin:$PATH
    # export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk.jdk/Contents/Home
    export JAVA_HOME=/Applications/Android\ Studio.app/Contents/jre/jdk/Contents/Home/
    # if $(docker-machine version 2>/dev/null >/dev/null)
    # then
    #     if [[ $(docker-machine status 2>/dev/null) -ne "Running" ]]
    #     then
    #         docker-machine start
    #     fi
    #     eval $(docker-machine env 2>/dev/null)
    # fi
else
    export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/
    export GPG_TTY=$(tty)
fi

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
# brew install bash-completion@2
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
[ -f ${GPG_AGENT} ] && eval "$(gpg-agent -q --daemon --log-file=~/.gnupg/gpg.log 2>/dev/null)"
# export SSH_AUTH_SOCK=~/.gnupg/S.gpg-agent.ssh   ## needed to cache ssh pass

if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent -t 1h > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [[ ! "$SSH_AUTH_SOCK" ]]; then
    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
fi

#
# Aliases
#
alias ..='cd .. && pwd'
alias a='ag'
alias am='ag -i --ignore=src/test --ignore=vendor'
alias cqlsh='cqlsh --no-color'
alias dl='cd ~/Downloads'
alias dstop='docker ps -a | grep -v CONTAINER | cut -d" " -f1 | xargs docker stop'
alias drm='docker ps -a | grep -v CONTAINER | cut -d" " -f1 | xargs docker rm'
alias dps='docker ps -a'
alias ed='cd ~/.emacs.d'
alias eb="$EDITOR ~/.bashrc"
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
alias gv='govendor'
alias h='history'
alias jo='jsonify'
alias j='jq'
alias kc='kubectl '
alias l='less -R'
alias la='ls -hlA'
alias ll='ls -hlA'
alias lt='ls -ltrA'
alias ltr='ls -ltrA'
alias m='make'
alias pa='ps aux | grep '
alias psa='ps aux'
alias r='rg --no-heading '
alias ssh="TERM=xterm-256color ssh"
alias td='tmux attach -d'

#
# Functions
#

function e() {
    if [ -t 0 ]
    then
	echo expecting input on stdin
	$EDITOR $@
    else
	tmpf=$(mktemp)
	while read line
	do
	    echo "$line" >> $tmpf
	done < /dev/stdin

	emacsclient -nw -e "(progn (find-file \"$tmpf\") (delete-other-windows) (ansi-colorize-buffer) (save-buffer))"
    fi
}

# always print pwd when changing dirs.
# if CDPATH is set it prints absolute path changes, but not `cd ..`
function cd() {
    builtin cd "$@" >/dev/null
    pwd
}

function tmp {
    if [ $# -eq 0 ] ; then
	echo "Expects single argument to name temporary folder."
	return
    fi

    cd ~/tmp
    md "$1-`date +%Y-%m-%dT%H:%M:%S`"
}

function em {
    $EDITOR --eval "(progn (magit-status \"$PWD/$(git rev-parse --show-cdup)\") (delete-other-windows))"
}

function man {
    emacsclient -nw -e "(progn (switch-to-buffer (man \"$1\") nil t) (delete-other-windows))"
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

function reload {
    . ~/.bashrc
    echo "loaded ~/.bashrc"
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

function set_title {
    printf "\033k$1\033\\"
}

function mosh {
    set_title "m[$*]"
    command mosh "$@"
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
    ag -ri "$@" $HOME/.history | cut -d":" -f2- | sort | uniq
}

function hr {
    rg --no-heading --no-filename -N "$@" $HOME/.history | sort | uniq
}

function hf {
    eval $( rg --no-heading --no-filename -N '^[^#]' $HOME/.history | awk '!x[$0]++' | fzf --preview='' )
}

function sum {
    while read n; do ((sum += n)); done; echo $sum;
}

function check-php-files {
    git ls-files -m | while read f ; do php -l $f ; done
}

function datediff() {
    d1=$(date -d "$1" +%s)
    d2=$(date -d "$2" +%s)
    echo $(( (d1 - d2) / 86400 )) days
}

function echoerr() {
    echo "$@" >&2
}

function z() {
    xz --compress --keep --threads=`nproc` --verbose "$@"
}


function up() {
    if [[ 'Darwin' == `uname` ]]
    then
        brew update && brew upgrade && brew cleanup
    else
        sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y
    fi
}
