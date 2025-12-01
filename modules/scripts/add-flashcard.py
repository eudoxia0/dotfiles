#!/usr/bin/env python
"""
Add a new flashcard to the inbox.
"""

import sys
from pathlib import Path

PATH: Path = Path.home() / "Root" / "1-Workspace" / "flashcards" / "inbox.txt"


def main():
    card: str = read_card()
    with open(PATH, "a+") as f:
        f.write(card + "\n")


def read_card() -> str:
    if len(sys.argv) > 1:
        return sys.argv[1]
    else:
        return input("Card: ").strip()


if __name__ == "__main__":
    main()
