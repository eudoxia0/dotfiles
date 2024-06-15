#!/usr/bin/env python
"""
Prefix every file in the current directory with its creation or modification
timestamp, whichever is oldest.

Skips files already prefixed.
"""
import os
from datetime import datetime

TIMESTAMP_FORMAT: str = "%Y-%m-%d-%H%M"

def get_oldest_timestamp(file_path: str) -> str:
    """
    Returns the oldest timestamp (either created or modified) of a file in YYYYMMDD-HHmm format.
    """
    created_at = datetime.fromtimestamp(os.path.getctime(file_path))
    modified_at = datetime.fromtimestamp(os.path.getmtime(file_path))
    oldest_timestamp = min(created_at, modified_at)
    return oldest_timestamp.strftime(TIMESTAMP_FORMAT)


def is_prefixed(filename: str) -> bool:
    """
    Returns True if the first four characters are digits (stripping dashes).
    """
    effective_filename: str = filename.replace("-", "")
    if len(effective_filename) < 4:
        return False
    return effective_filename[0:4].isdigit()

def prefix_with_timestamp(directory: str) -> None:
    for filename in os.listdir(directory):
        file_path = os.path.join(directory, filename)

        # Skip directories
        if os.path.isdir(file_path):
            continue

        if not is_prefixed(filename):
            oldest_timestamp = get_oldest_timestamp(file_path)
            new_filename = f"{oldest_timestamp} {filename}"
            new_file_path = os.path.join(directory, new_filename)
            os.rename(file_path, new_file_path)

if __name__ == "__main__":
    prefix_with_timestamp(".")
