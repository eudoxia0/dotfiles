#!/bin/sh

if [ -e /tmp/emacs-shared/server ]; then
    emacsclient -c -s /tmp/emacs-shared/server
else
    emacs
fi
