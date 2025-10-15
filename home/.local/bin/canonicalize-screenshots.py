#!/usr/bin/env python3
"""
Rewrites screenshot filenames from macOS format to

    YYYY-MM-DD-HHMMSS_macos.png
"""

import os
import re
from datetime import datetime

# Compile a regular expression pattern to match the original filename format
pattern = re.compile(
    r"^Screenshot (\d{4}-\d{2}-\d{2}) at (\d{1,2}).(\d{2}).(\d{2})\s([apAP][mM])\.png$"
)


def convert_filename(filename):
    match = pattern.match(filename)
    if match:
        date, hour, minute, second, meridiem = match.groups()
        # Convert to 24-hour format
        hour_24 = datetime.strptime(f"{hour} {meridiem}", "%I %p").strftime("%H")
        # Construct new filename
        new_filename = f"{date}-{hour_24}{minute}{second}_macos.png"
        return new_filename
    return None


def rename_files():
    for filename in os.listdir("."):
        new_filename = convert_filename(filename)
        if new_filename:
            os.rename(filename, new_filename)


rename_files()
