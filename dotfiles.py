#!/usr/bin/env python3
import sys
from pathlib import Path

DOTFILES = [
    ".config/ghostty/config",
    ".config/zed/settings.json",
    ".emacs.d/init.el",
    ".gitconfig",
    ".global-gitignore",
    ".local/bin/backup.sh",
    ".local/bin/canonicalize-screenshots.py",
    ".local/bin/decrypt.sh",
    ".local/bin/encrypt.sh",
    ".local/bin/probe-port.sh",
    ".local/bin/timestamp.py",
    ".zshrc",
]

REPO_DIR = Path(__file__).parent.resolve()
HOME_DIR = Path.home()

def apply():
    for file in DOTFILES:
        src = REPO_DIR / "home" / file
        dst = HOME_DIR / file
        dst.parent.mkdir(parents=True, exist_ok=True)
        if dst.exists() or dst.is_symlink():
            dst.unlink()
        dst.symlink_to(src)
        print(f"✓ {file}")


def clean():
    for file in DOTFILES:
        dst = HOME_DIR / file
        if dst.is_symlink():
            dst.unlink()
            print(f"✓ {file}")

def usage():
    print("Usage: ./dotfiles.py <apply|clean>")
    sys.exit(1)

def main():
    if len(sys.argv) != 2:
        usage()

    if sys.argv[1] == "apply":
        apply()
    elif sys.argv[1] == "clean":
        clean()
    else:
        usage()

if __name__ == "__main__":
    main()
