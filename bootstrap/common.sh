#!/usr/bin/env bash

# Node

sudo npm -g install jslint

# Lisp
curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
  --eval '(quicklisp-quickstart:install :path ".quicklisp")' \
  --quit
sbcl --eval '(ql:quickload :quicklisp-slime-helper)' --quit
