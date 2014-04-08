# Install CIM
CIM_HOME=HOME/.cim

curl https://raw.github.com/KeenS/CIM/master/scripts/cim_installer | /bin/sh

# Install various lisps

cim install sbcl-1.1.16
cim install clisp-2.49
cim install ccl-1.9

cim use sbcl-1.1.16
