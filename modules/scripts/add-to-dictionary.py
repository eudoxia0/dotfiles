#!/usr/bin/env python
"""
Add a new word to the Firefox dictionary.
"""

import sys
from pathlib import Path

PATH: Path = Path.home() / "dotfiles" / "modules" / "firefox" / "words.txt"


def main():
    ws: set[str] = load_words()
    ws.add(read_word())
    write_words(sorted(ws))


def load_words() -> set[str]:
    words: set[str] = set()
    with open(PATH, "r") as stream:
        for line in stream:
            line: str = line.strip()
            if line:
                words.add(line)
    return words


def write_words(ws: list[str]):
    with open(PATH, "w") as f:
        f.write("\n".join(ws))
        f.write("\n")


def read_word() -> str:
    if len(sys.argv) > 1:
        return sys.argv[1]
    else:
        return input("New Word: ").strip()


if __name__ == "__main__":
    main()
