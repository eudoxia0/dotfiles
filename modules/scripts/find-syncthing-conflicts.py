#!/usr/bin/env python
"""Find Syncthing conflict files recursively in a directory."""

import os
import sys
from pathlib import Path


def find_conflict_files(root_dir: Path = Path.cwd()) -> list[Path]:
    conflict_files: list[Path] = []
    for root, _, files in os.walk(root_dir):
        for filename in files:
            if ".sync-conflict-" in filename:
                conflict_files.append(Path(root) / filename)
    return conflict_files


def main() -> int:
    search_dir: Path = Path(sys.argv[1]) if len(sys.argv) > 1 else Path.cwd()

    if not search_dir.exists():
        print(f"Error: Directory '{search_dir}' does not exist", file=sys.stderr)
        return 1

    conflict_files: list[Path] = find_conflict_files(search_dir)
    for conflict_file in sorted(conflict_files):
        print(conflict_file)

    return 0


if __name__ == "__main__":
    sys.exit(main())
