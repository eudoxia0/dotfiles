#!/usr/bin/env python
"""
Read a text from stdin, strip all text except punctuation.
"""

import sys

PUNCT: set[str] = {".", ",", ";", ":", "?", "!", "-", "–", "—", "(", ")"}

for c in sys.stdin.read():
    if c in PUNCT:
        print(c, end="")
print()


def main(text: str):
    for c in text:
        if c in PUNCT:
            print(c, end="")
    print()


if __name__ == "__main__":
    argc: int = len(sys.argv)
    if argc == 2:
        with open(sys.argv[1]) as stream:
            main(stream.read())
    elif argc == 1:
        main(sys.stdin.read())
    else:
        print("Wrong number of arguments.", file=sys.stderr)
        exit(-1)
