#!/usr/bin/env bash

rsync -avh --exclude "README.md" --exclude ".git/" --exclude "setup.sh" . ~
