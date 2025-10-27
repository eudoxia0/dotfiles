#!/usr/bin/env python3
"""
Rewrites screenshot filenames from macOS format to

    YYYY-MM-DD-HHMMSS_macos.png

And moves them.
"""

import os
import shutil
import re
from datetime import datetime
from pathlib import Path

# Compile a regular expression pattern to match the original filename format
pattern: re.Pattern[str] = re.compile(
    r"^Screenshot (\d{4}-\d{2}-\d{2}) at (\d{1,2}).(\d{2}).(\d{2})\s([apAP][mM])\.png$"
)

BASE_DIR: Path = Path("/Volumes/Asterion/Root/4 Library/Images/Screenshots/Desktop")


def convert_filename(filename: str):
    match = pattern.match(filename)
    if match is not None:
        date, hour, minute, second, meridiem = match.groups()
        # Convert to 24-hour format.
        assert isinstance(hour, str)
        assert isinstance(meridiem, str)
        hour_24: str = datetime.strptime(f"{hour} {meridiem}", "%I %p").strftime("%H")
        # Construct new filename.
        assert isinstance(date, str)
        assert isinstance(minute, str)
        assert isinstance(second, str)
        new_filename: str = f"{date}-{hour_24}{minute}{second}_macos.png"
        # Extract year from date
        year: str = date.split("-")[0]
        return new_filename, year
    else:
        return None, None


def rename_and_move_files():
    for filename in os.listdir("."):
        new_filename, year = convert_filename(filename)
        if new_filename and year:
            # Create year directory if it doesn't exist
            year_dir = BASE_DIR / year
            year_dir.mkdir(parents=True, exist_ok=True)

            # Construct full destination path
            dest_path = year_dir / new_filename

            # Move and rename the file
            _ = shutil.move(filename, dest_path)
            print(f"Moved: {filename} -> {dest_path}")


if __name__ == "__main__":
    rename_and_move_files()
