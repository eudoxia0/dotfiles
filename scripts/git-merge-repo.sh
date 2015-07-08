#!/usr/bin/env bash

# git-merge-repo: Merge a repository into another's subdirectory
# usage: git-merge-repo [name] [absolute path]

NAME=$1
PATH=$2

git remote add $NAME $PATH
git fetch $NAME
git merge -s ours --no-commit $NAME/master
git read-tree --prefix=$NAME/ -u $NAME/master
git commit
