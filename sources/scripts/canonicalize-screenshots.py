#!/usr/bin/env python
"""
Iterates over all files in a directory. If a file is in Pantheon screenshot
format, e.g.:

```
Screenshot from 2023-11-23 21.16.23.png
Screenshot from 2023-11-23 21-16-23.png
```

It rewrites it to the following format:

```
2023-11-23-211623_pantheon.png
```
"""
import os
import re

PANTHEON_REGEX = re.compile(r"^Screenshot from (\d{4})-(\d{2})-(\d{2}) (\d{2})[\.\-](\d{2})[\.\-](\d{2})[\.\-]png$")

def is_screenshot(filename: str) -> bool:
    """
    Returns True if the filename matches the Pantheon screenshot format.
    """
    return PANTHEON_REGEX.match(filename) is not None

def canonicalize_screenshot(filename: str) -> str:
    """
    Returns the canonicalized filename.
    """
    match = PANTHEON_REGEX.match(filename)
    year, month, day, hour, minute, second = match.groups()
    return f"{year}-{month}-{day}-{hour}{minute}{second}_pantheon.png"

def canonicalize_screenshots(directory: str) -> None:
    """
    Canonicalizes all screenshots in a directory.
    """
    for filename in os.listdir(directory):
        file_path = os.path.join(directory, filename)

        # Skip directories
        if os.path.isdir(file_path):
            continue

        if is_screenshot(filename):
            new_filename = canonicalize_screenshot(filename)
            new_file_path = os.path.join(directory, new_filename)
            os.rename(file_path, new_file_path)

if __name__ == "__main__":
    canonicalize_screenshots(".")