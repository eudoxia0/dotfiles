#!/usr/bin/env python2

from __future__ import print_function
import sys
from bs4 import BeautifulSoup

bookmarks = BeautifulSoup(open('bookmarks.html').read())


print('Tagging data we want to extract...', file=sys.stderr)
for node in bookmarks.find_all('h3') + bookmarks.find_all('a'):
    node['class'] = 'extractme'
print('Extracting data', file=sys.stderr)
for node in bookmarks.find_all(['h3', 'a'], {'class': 'extractme'}):
    if node.name == 'h3':
        print('- ' + node.text.encode('utf8') + ':')
    else:
        print('    - "' + node.text.encode('utf8') + '": "' + node['href'] + '"')
