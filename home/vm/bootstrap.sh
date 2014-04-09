#!/bin/sh

veewee vbox build 'ubuntu-13.10' -a -n
veewee vbox build 'openbsd-5.3' -a -n

veewee vbox export 'openbsd-5.3'
