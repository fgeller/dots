#!/usr/bin/env bash

NOTMUCH=/usr/local/bin/notmuch

$NOTMUCH search --output=files tag:archive and folder:m2l/INBOX  | grep INBOX | while read m ; do mv -v "$m" /Users/fgeller/Mail/m2l/Archive/cur/ ; done
$NOTMUCH search --output=files folder:m2l/INBOX and not tag:m2l-in | grep INBOX | xargs rm -v
$NOTMUCH search --output=files "folder:m2l/Spam" and not tag:m2l-spam | grep Spam | xargs -I {} rm -v "{}"

$NOTMUCH search --output=files folder:gmail/INBOX and not tag:inbox | grep INBOX | xargs rm -v
$NOTMUCH search --output=files "folder:gmail/[Gmail].Spam" and not tag:spam | grep Spam | xargs -I {} rm -v "{}"

$NOTMUCH tag -m2l-in tag:m2l-in and not folder:m2l/INBOX

$NOTMUCH new

$NOTMUCH tag +m2l-in   folder:m2l/INBOX
$NOTMUCH tag +archive  folder:m2l/Archive
$NOTMUCH tag +spam     folder:m2l/Spam

$NOTMUCH tag +inbox folder:gmail/INBOX
$NOTMUCH tag +spam "folder:gmail/[Gmail].Spam"
