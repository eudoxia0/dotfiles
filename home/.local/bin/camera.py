#!/usr/bin/env python3
'''
Rewrites images and videos:

    IMG_YYYYMMDD_HHMMSS.jpg -> YYYY-MM-DD-HHMMSS.jpg
    VID_YYYYMMDD_HHMMSS.mp4 -> YYYY-MM-DD-HHMMSS.mp4

Any trailing strings are retained.
'''
import os
import re
from datetime import datetime

PATTERN = re.compile(r'^(\w{3,4})_(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})(\d{2})(\w*)\.(\w{3})$')

def match(filename):
    match = PATTERN.match(filename)
    if match:
        groups = match.groups()
        return groups
    else:
        return None

def rename(groups):
    type, y, m, d, h, min, s, junk, ext = groups
    return f'{y}-{m}-{d}-{h}{min}{s}{junk}.{ext}'

def tests():
    assert match('IMG_20000102_010203.jpg') == ('IMG', '2000', '01', '02', '01', '02', '03', '', 'jpg')
    assert match('IMG_20000102_010203_1.jpg') == ('IMG', '2000', '01', '02', '01', '02', '03', '_1', 'jpg')
    assert match('IMG_20000102_010203_HHT.jpg') == ('IMG', '2000', '01', '02', '01', '02', '03', '_HHT', 'jpg')
    assert match('VID_20000102_010203.mp4') == ('VID', '2000', '01', '02', '01', '02', '03', '', 'mp4')
    assert rename(match('IMG_20000102_010203.jpg')) == '2000-01-02-010203.jpg'
    assert rename(match('IMG_20000102_010203_1.jpg')) == '2000-01-02-010203_1.jpg'
    assert rename(match('IMG_20000102_010203_HHT.jpg')) == '2000-01-02-010203_HHT.jpg'
    assert rename(match('VID_20000102_010203.mp4')) == '2000-01-02-010203.mp4'

def main():
    for filename in sorted(os.listdir('.')):
        groups = match(filename)
        if groups:
            new_name = rename(groups)
            os.rename(filename, new_name)

if __name__ == '__main__':
    tests()
    main()
