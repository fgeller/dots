#!/usr/bin/env bash
set -e
target=$HOME/dots
function ensure_link {
    local fn=".$1"
    if [[ ! -L "$HOME/$fn" || $(readlink "$HOME/$fn") != "$target/$fn" ]]
    then
	echo refreshing link for $fn: $(ln -vfs "$target/$fn" "$HOME/$fn")
    else
	echo proper link for $fn: $(readlink $HOME/$fn)
    fi
}
for f in bash_gpg bash_profile bashrc gitconfig globalgitignore inputrc notmuch-config offlineimap.py offlineimaprc sbtconfig tmux.conf
do
    ensure_link $f
done
