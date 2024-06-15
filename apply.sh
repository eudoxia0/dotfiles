#!/bin/sh

set -e
set -u
set -o pipefail

nix build .#darwinConfigurations.metauro.system
./result/sw/bin/darwin-rebuild switch --flake .#metauro
