#!/usr/bin/env python
"""
Add a new feed to the feedlist.
"""

import sys
from pathlib import Path

PATH: Path = (
    Path.home()
    / "root"
    / "1-Workspace"
    / "dotfiles"
    / "modules"
    / "newsboat"
    / "urls.txt"
)


def main():
    fs: set[str] = load_feeds()
    fs.add(read_feed())
    write_feeds(sorted(fs))


def load_feeds() -> set[str]:
    feeds: set[str] = set()
    with open(PATH, "r") as stream:
        for line in stream:
            line: str = line.strip()
            if line:
                feeds.add(line)
    return feeds


def write_feeds(fs: list[str]):
    with open(PATH, "w") as f:
        f.write("\n".join(fs))
        f.write("\n")


def read_feed() -> str:
    if len(sys.argv) > 1:
        return sys.argv[1]
    else:
        return input("New Feed: ").strip()


if __name__ == "__main__":
    main()
