#!/usr/bin/env -S pyenv exec python
"""
# Overview

A script to turn a list into a CSV of Mochi cards.

For example if you give it a list of newline-separated names of US presidents it
makes cards that ask questions like:

1. What has position 1? George Washington.
2. What position is George Washington? 2.
3. What comes before James Monroe? James Madison.
4. What comes after James Monroe? John Quincy Adams.

Based on gwern's:

- https://www.gwern.net/haskell/mnemo4.hs
- https://www.gwern.net/Spaced-repetition

# Input Format

- Reads from stdin
- The first line is a title or tag
- The rest is a newline-separated list of strings

# Mochi Template

You should import these into a deck with a template having the fields:

- `Tag`
- `Front`
- `Back`

Example usage:

```
$ echo -e 'Greek Alphabet\nAlpha\nBeta\nGamma' | ./ordinal.py
Tag,Front,Back
"Greek Alphabet","What has position 1?","Alpha"
"Greek Alphabet","What has position 2?","Beta"
"Greek Alphabet","What has position 3?","Gamma"
"Greek Alphabet","What position is Alpha?","1"
"Greek Alphabet","What position is Beta?","2"
"Greek Alphabet","What position is Gamma?","3"
"Greek Alphabet","What comes before Beta?","Alpha"
"Greek Alphabet","What comes before Gamma?","Beta"
"Greek Alphabet","What comes after Alpha?","Beta"
"Greek Alphabet","What comes after Beta?","Gamma"
```
"""
from dataclasses import dataclass
import sys

@dataclass(frozen=True)
class Item:
    index: int
    text: str

    def __post_init__(self):
        assert self.index > 0

@dataclass(frozen=True)
class Card:
    tag: str
    front: str
    back: str

def main():
    lines: list[str] = sys.stdin.readlines()
    tag: str = lines[0].strip()
    items: list[Item] = make_items(lines[1:])
    forward: list[Card] = [forward_card(tag, item) for item in items]
    backward: list[Card] = [backward_card(tag, item) for item in items]
    prev: list[Card | None] = [previous_card(tag, items, item) for item in items]
    succ: list[Card | None] = [successor_card(tag, items, item) for item in items]
    cards: list[Card] = forward + backward + nonnull(prev) + nonnull(succ)
    print("Tag,Front,Back")
    for card in cards:
        print(f'"{card.tag}","{card.front}","{card.back}"')

def make_items(texts: list[str]) -> list[Item]:
    return [
        Item(index=index+1, text=text.strip())
        for index, text in enumerate(texts)
    ]

def forward_card(tag: str, item: Item) -> Card:
    return Card(
        tag=tag,
        front=f"What has position {item.index}?",
        back=item.text,
    )

def backward_card(tag: str, item: Item) -> Card:
    return Card(
        tag=tag,
        front=f"What position is {item.text}?",
        back=str(item.index),
    )

def previous_card(tag: str, items: list[Item], item: Item) -> Card | None:
    prev: Item | None = previous(items, item)
    if prev is None:
        return None
    else:
        return Card(
            tag=tag,
            front=f"What comes before {item.text}?",
            back=prev.text,
        )

def successor_card(tag: str, items: list[Item], item: Item) -> Card | None:
    succ: Item | None = successor(items, item)
    if succ is None:
        return None
    else:
        return Card(
            tag=tag,
            front=f"What comes after {item.text}?",
            back=succ.text,
        )

def previous(items: list[Item], item: Item) -> Item | None:
    return find_by_index(items, item.index - 1)

def successor(items: list[Item], item: Item) -> Item | None:
    return find_by_index(items, item.index + 1)

def find_by_index(items: list[Item], idx: int) -> Item | None:
    filtered: list[Item] = [item for item in items if item.index == idx]
    if filtered:
        return filtered[0]
    else:
        return None

def nonnull(cards: list[Card | None]) -> list[Card]:
    return [card for card in cards if card is not None]

if __name__ == "__main__":
    main()
