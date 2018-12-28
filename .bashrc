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

export PATH=~/bin:/usr/local/opt/gnu-tar/libexec/gnubin:/usr/local/go/bin:$GOPATH/bin:$GOROOT/bin:/usr/local/bin:/usr/local/sbin:/usr/texbin:~/.local/bin:$PATH
export CDPATH=".:~:~/src/github.com:~/src/github.com/fgeller:~/src/github.com/movio"

export EDITOR="emacs -nw"

if [[ 'Darwin' == `uname` ]]
then
    GPG_AGENT=$(which gpg-agent)
    GPG_TTY=`tty`
    export GPG_TTY
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
    export PATH=/usr/local/opt/gnu-tar/libexec/gnubin:/usr/local/opt/coreutils/libexec/gnubin:$PATH
    export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_121.jdk/Contents/Home
    if $(docker-machine version 2>/dev/null >/dev/null)
    then
        echo found it
        if [[ $(docker-machine status) -ne "Running" ]]
        then
            docker-machine start
        fi
        eval $(docker-machine env)
        echo loaded docker-machine env
    fi
else
    export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64/
    export GPG_TTY=$(tty)
fi

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
[ -f ${GPG_AGENT} ] && [[ $(gpg-connect-agent /bye) -ne 0 ]] && eval "$(gpg-agent -q --daemon --log-file=~/.gnupg/gpg.log)"
[ -f ~/.travis/travis.sh ] && source ~/.travis/travis.sh

#
# NVM (across darwin and linux)
#
[ -f ~/.nvm ] && export NVM_DIR="$HOME/.nvm"
[ -f /usr/local/opt/nvm/nvm.sh ] && . "/usr/local/opt/nvm/nvm.sh"
[ -f ~/.nvm/nvm.sh ] && . "${HOME}/.nvm/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

#
# Aliases
#
alias ..='cd .. && pwd'
alias a='rg -i'
alias am='ag -i --ignore=src/test --ignore=vendor'
alias cqlsh='cqlsh --no-color'
alias dl='cd ~/Downloads'
alias dstop='docker ps -a | grep -v CONTAINER | cut -d" " -f1 | xargs docker stop'
alias drm='docker ps -a | grep -v CONTAINER | cut -d" " -f1 | xargs docker rm'
alias dps='docker ps -a'
alias e="$EDITOR"
alias ed='cd ~/.emacs.d'
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
alias gvf='govendor fetch -v'
alias gvs='govendor sync -v'
alias h='history'
alias jo='jsonify'
alias j='jq'
alias kc='kubectl '
alias kdev='kubectl --context=movio-dev'
alias kfr='kubectl --context=movio-prod-fr'
alias keu='kubectl --context=movio-prod-eu'
alias kus='kubectl --context=movio-prod-us'
alias kcn='kubectl --context=movio-prod-cn'
alias l='less -R'
alias la='ls -hlA'
alias ll='ls -hlA'
alias lt='ls -ltrA'
alias ltr='ls -ltrA'
alias m='make'
alias pa='ps aux | grep '
alias psa='ps aux'
alias r='rg --no-heading '
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

function em {
    $EDITOR --eval "(progn (magit-status \"$PWD/$(git rev-parse --show-cdup)\") (delete-other-windows))"
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
    ag -ri "$@" $HOME/.history | cut -d":" -f2- | sort | uniq
}

function hr {
    rg --no-heading --no-filename -N "$@" $HOME/.history | sort | uniq
}

function sum {
    while read n; do ((sum += n)); done; echo $sum;
}

function mca {
    kt consume -topic mc.red.campaigns.event -brokers $1 -offsets all=newest-10: | jq '.value |= fromjson | .value.created |= (. / 1000 / 1000 / 1000 | todate) | .value'
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

function kget() {
    echo ======= cn ======= >&2
    kcn get $1 $2

    echo ======= fr ======= >&2
    kfr get $1 $2

    echo ======= us ======= >&2
    kus get $1 $2
}

function kscale() {
    echo ======= cn ======= >&2
    kcn scale --replicas=$3 $1 $2

    echo ======= fr ======= >&2
    kfr scale --replicas=$3 $1 $2

    echo ======= us ======= >&2
    kus scale --replicas=$3 $1 $2
}

function krm() {
    echo ======= cn ======= >&2
    kcn delete $1 $2

    echo ======= fr ======= >&2
    kfr delete $1 $2

    echo ======= us ======= >&2
    kus delete $1 $2
}

function kapply() {
    echo ======= cn ======= >&2
    kcn apply -f kube.deploy.prod-cn.yaml

    echo ======= fr ======= >&2
    kfr apply -f kube.deploy.prod-fr.yaml

    echo ======= us ======= >&2
    kus apply -f kube.deploy.prod-us.yaml
}

function z() {
    xz --compress --keep --threads=`nproc` --verbose "$@"
}


function up() {
    sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y
}
