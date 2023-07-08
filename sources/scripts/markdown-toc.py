#!/usr/bin/env python3

import re
import sys

with open(sys.argv[1], 'r') as file:
    lines = file.readlines()
    assert len(lines) > 0

    pattern = re.compile(r'^(#{1,6}) (.*?) {#(.*?)}$')

    for line in lines:
        match = pattern.match(line)
        if match:
            level: int = len(match.group(1))
            name: str = match.group(2)
            link: str = match.group(3)
            print(f'{"    " * (level - 1)}1. [{name}](#{link})')
