#!/usr/bin/env python
"""Find Syncthing conflict files recursively in a directory."""

import os
import sys
from pathlib import Path
from typing import List


def find_conflict_files(root_dir: Path = Path.cwd()) -> List[Path]:
    """
    Find all Syncthing conflict files recursively.
    """
    conflict_files: List[Path] = []

    for root, dirs, files in os.walk(root_dir):
        for filename in files:
            if ".sync-conflict-" in filename:
                conflict_files.append(Path(root) / filename)

    return conflict_files


def main() -> int:
    """Main entry point."""
    search_dir = Path(sys.argv[1]) if len(sys.argv) > 1 else Path.cwd()

    if not search_dir.exists():
        print(f"Error: Directory '{search_dir}' does not exist", file=sys.stderr)
        return 1

    print(f"Searching for Syncthing conflict files in: {search_dir}")
    print()

    conflict_files = find_conflict_files(search_dir)

    for conflict_file in sorted(conflict_files):
        print(conflict_file)

    print()
    print(f"Total conflict files found: {len(conflict_files)}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
