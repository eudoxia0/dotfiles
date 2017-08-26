#!/usr/bin/env bash

sbcl --noinform --load ~/.sbclrc --script $1 ${@:2}
