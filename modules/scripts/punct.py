#!/usr/bin/env python
"""
Read a text from stdin, strip all text except punctuation.
"""

import sys

PUNCT: set[str] = {".", ",", ";", ":", "?", "!", "-", "–", "—", "(", ")"}


def main(text: str):
    for c in text:
        if c in PUNCT:
            print(c, end="")
    print()


if __name__ == "__main__":
    argc: int = len(sys.argv)
    print(sys.argv)
    if argc == 1:
        with open(sys.argv[0]) as stream:
            main(stream.read())
    elif argc == 0:
        main(sys.stdin.read())
    else:
        print("Wrong number of arguments.", file=sys.stderr)
        exit(-1)
