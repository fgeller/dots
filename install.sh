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
for f in emacs.d gitconfig gitmsg.txt globalgitignore inputrc tmux.conf zshrc zsh psqlrc jjconfig.toml
do
    ensure_link $f
done

for d in alacritty helix zellij ghostty
do
	  echo refreshing link for $d: $(ln -vfs "$target/.config/$d" "$HOME/.config/$d")
done

/usr/bin/tic -x -o ~/.terminfo xterm-24bit.terminfo
