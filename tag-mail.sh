#!/usr/bin/env bash

NOTMUCH=/usr/local/bin/notmuch

$NOTMUCH search --output=files folder:m2l/INBOX and not tag:m2l-in | grep INBOX | xargs rm -v
$NOTMUCH search --output=files "folder:m2l/Spam" and not tag:m2l-spam | grep Spam | xargs -I {} rm -v "{}"

$NOTMUCH search --output=files folder:gmail/INBOX and not tag:inbox | grep INBOX | xargs rm -v
$NOTMUCH search --output=files "folder:gmail/[Gmail].Spam" and not tag:spam | grep Spam | xargs -I {} rm -v "{}"

$NOTMUCH search --output=files folder:movio/INBOX and not tag:movio-in | grep INBOX | xargs rm -v
$NOTMUCH search --output=files "folder:movio/[Gmail].Spam" and not tag:movio-spam | grep Spam | xargs -I {} rm -v "{}"

$NOTMUCH new

$NOTMUCH tag +m2l-in folder:m2l/INBOX
$NOTMUCH tag +spam "folder:m2l/Spam"

$NOTMUCH tag +inbox folder:gmail/INBOX
$NOTMUCH tag +spam "folder:gmail/[Gmail].Spam"

$NOTMUCH tag +movio-in folder:movio/INBOX
$NOTMUCH tag +movio-spam "folder:movio/[Gmail].Spam"
