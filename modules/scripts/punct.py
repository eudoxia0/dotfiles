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
