#!/usr/bin/env bash
set -e
target=$HOME/src/github.com/fgeller/dots
function ensure_link {
    local fn=".$1"
    if [[ ! -L "$HOME/$fn" || $(readlink "$HOME/$fn") != "$target/$fn" ]]
    then
	echo refreshing link for $fn: $(ln -vfs "$target/$fn" "$HOME/$fn")
    else
	echo proper link for $fn: $(readlink $HOME/$fn)
    fi
}
for f in bash_profile bashrc emacs.d gitconfig gitmsg.txt globalgitignore inputrc tmux.conf
do
    ensure_link $f
done
