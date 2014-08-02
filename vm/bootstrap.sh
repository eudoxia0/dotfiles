#!/bin/sh

vagrant plugin install vagrant-guests-openbsd

veewee vbox build 'openbsd-5.3' -a -n
veewee vbox export 'openbsd-5.3'
vagrant box add 'openbsd-5.3' 'openbsd-5.3.box'

veewee vbox build 'centos-6.5' -a -n

veewee vbox build 'freebsd-9.1' -a -n
