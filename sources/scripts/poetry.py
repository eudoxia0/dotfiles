#!/usr/bin/env -S pyenv exec python
"""
# Overview

A script to turn a poem into a CSV of Mochi cards.

Based on gwern's:

    https://www.gwern.net/haskell/mnemo.hs

    https://www.gwern.net/Spaced-repetition

# Input Format

- Reads from stdin.
- First line is the poem's title.
- Second line is the author.
- Remaining lines are the poem.

# Mochi Template

The deck should have a template with these fields:

- `Title`
- `Author`
- `Context1`
- `Context2`
- `Line`

# Example Usage

```
$ cat wasteland.txt
The Waste Land
T. S. Eliot
April is the cruellest month, breeding
Lilacs out of the dead land, mixing
Memory and desire, stirring

$ cat wasteland.txt | ./poetry.py
Title,Author,Context1,Context2,Line
"The Waste Land","T. S. Eliot","","_Beginning_","April is the cruellest month, breeding"
"The Waste Land","T. S. Eliot","_Beginning_","April is the cruellest month, breeding","Lilacs out of the dead land, mixing"
"The Waste Land","T. S. Eliot","April is the cruellest month, breeding","Lilacs out of the dead land, mixing","Memory and desire, stirring"
```
"""
from dataclasses import dataclass
import sys

@dataclass(frozen=True)
class Line:
    # The line's position in the poem, zero-indexed.
    index: int
    # The text of the poem's line.
    text: str

    def __post_init__(self):
        assert self.index >= 0

@dataclass(frozen=True)
class Card:
    title: str
    author: str
    context1: str
    context2: str
    line: str

def main():
    lines: list[str] = sys.stdin.readlines()
    title: str = lines[0].strip()
    author: str = lines[1].strip()
    poem: list[Line] = make_lines(lines[2:])
    cards: list[Card] = [make_card(title, author, line, poem) for line in poem]
    print_cards(title, cards)

def make_lines(texts: list[str]) -> list[Line]:
    return [
        Line(index=index, text=text.strip())
        for index, text in enumerate(texts)
    ]

def make_card(title: str, author: str, line: Line, all_lines: list[Line]) -> Card:
    context1: str
    context2: str
    if line.index == 0:
        # First line of the poem.
        context1 = ""
        context2 = "_Beginning_"
    elif line.index == 1:
        # Second line of the poem.
        context1 = "_Beginning_"
        context2 = all_lines[0].text
    else:
        context1 = all_lines[line.index-2].text
        context2 = all_lines[line.index-1].text

    main_line: str = line.text

    def encode(s: str) -> str:
        return s.replace("\"", "\\")

    return Card(
        title=title,
        author=author,
        context1=encode(context1),
        context2=encode(context2),
        line=encode(main_line),
    )

def print_cards(title: str, cards: list[Card]):
    print("Title,Author,Context1,Context2,Line")
    for card in cards:
        print(f"\"{card.title}\",\"{card.author}\",\"{card.context1}\",\"{card.context2}\",\"{card.line}\"")

if __name__ == "__main__":
    main()
