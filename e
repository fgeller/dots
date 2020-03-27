#!/usr/local/bin/bash

if [ -t 0 ]
then
   echo expecting input on stdin
   emacs -nw $@
else
    tmpf=$(mktemp)

    echo ">> reading stdin to tempfile $tmpf"
    while read line
    do
	echo "$line" >> $tmpf
    done < /dev/stdin

    echo ">> passing tempfile to emacs"
    emacs -nw --file "$tmpf" < /dev/tty > /dev/tty
fi
