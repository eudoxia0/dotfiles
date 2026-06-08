#!/usr/bin/env python
"""
Recursively count the number of nodes (files + directories) in a given directory.
"""

import os
import argparse
from pathlib import Path


def count_nodes(directory: Path, ignore_dirs: set[str]) -> tuple[int, int, int]:
    """
    Count files and directories recursively.

    Args:
        directory: Path to the directory to count
        ignore_dirs: Set of directory names to ignore

    Returns:
        Tuple of (total_nodes, file_count, dir_count)
    """
    file_count = 0
    dir_count = 0

    try:
        for entry in directory.iterdir():
            if entry.is_dir():
                # Skip ignored directories
                if entry.name in ignore_dirs:
                    continue

                dir_count += 1
                # Recursively count in subdirectories
                sub_files, sub_dirs, _ = count_nodes(entry, ignore_dirs)
                file_count += sub_files
                dir_count += sub_dirs
            else:
                file_count += 1
    except PermissionError:
        print(f"Warning: Permission denied for {directory}")

    total_nodes = file_count + dir_count
    return file_count, dir_count, total_nodes


def main():
    parser = argparse.ArgumentParser(
        description="Recursively count nodes (files + directories) in a directory"
    )
    parser.add_argument(
        "directory",
        nargs="?",
        default=".",
        help="Directory to count (default: current directory)"
    )
    parser.add_argument(
        "--ignore",
        action="store_true",
        help="Ignore .git, target, and node_modules directories"
    )

    args = parser.parse_args()

    # Set up ignored directories
    ignore_dirs = set()
    if args.ignore:
        ignore_dirs = {".git", "target", "node_modules"}

    # Resolve the directory path
    directory = Path(args.directory).resolve()

    if not directory.exists():
        print(f"Error: Directory '{directory}' does not exist")
        return 1

    if not directory.is_dir():
        print(f"Error: '{directory}' is not a directory")
        return 1

    # Count nodes
    file_count, dir_count, total_nodes = count_nodes(directory, ignore_dirs)

    # Display results
    print(f"Directory: {directory}")
    if ignore_dirs:
        print(f"Ignoring: {', '.join(sorted(ignore_dirs))}")
    print(f"\nFiles:       {file_count:,}")
    print(f"Directories: {dir_count:,}")
    print(f"Total nodes: {total_nodes:,}")

    return 0


if __name__ == "__main__":
    exit(main())
