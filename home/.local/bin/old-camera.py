#!/usr/bin/env python3
'''
Rewrites images and videos:

    YYYYMMDD_HHMMSS.jpg -> YYYY-MM-DD-HHMMSS.jpg
'''
import os
import re
from datetime import datetime

PATTERN = re.compile(r'^(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})(\d{2})\.jpg$')

def match(filename):
    match = PATTERN.match(filename)
    if match:
        groups = match.groups()
        return groups
    else:
        return None

def rename(groups):
    type, y, m, d, h, min, s = groups
    return f'{y}-{m}-{d}-{h}{min}{s}.jpg'

def tests():
    assert match('20000102_010203.jpg') == ('2000', '01', '02', '01', '02', '03')
    assert rename(match('20000102_010203.jpg')) == '2000-01-02-010203.jpg'

def main():
    for filename in sorted(os.listdir('.')):
        groups = match(filename)
        if groups:
            new_name = rename(groups)
            os.rename(filename, new_name)

if __name__ == '__main__':
    tests()
    main()
