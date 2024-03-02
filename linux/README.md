# Linux dotfiles

My dotfiles, using [Nix][nix].

[nix]: https://nixos.org/

Blog post: [_NixOS for the Impatient_][blog].

[blog]: https://borretti.me/article/nixos-for-the-impatient

## Setup

Run:

```bash
$ sudo nix-channel --add https://github.com/nix-community/home-manager/archive/release-23.05.tar.gz home-manager
$ sudo nix-channel --update
$ ./recrank.sh
```

To update the channel:

```bash
$ sudo nix-channel --update
```
