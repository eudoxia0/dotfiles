#!/usr/bin/env python
"""
Show node counts for all directories (like du for disk usage).
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
        Tuple of (file_count, dir_count, total_nodes)
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
        pass  # Silently skip directories we can't read

    total_nodes = file_count + dir_count
    return file_count, dir_count, total_nodes


def main():
    parser = argparse.ArgumentParser(
        description="Show node counts for directories (like du for disk usage)"
    )
    parser.add_argument(
        "directory",
        nargs="?",
        default=".",
        help="Directory to analyze (default: current directory)"
    )
    parser.add_argument(
        "--ignore",
        action="store_true",
        help="Ignore .git, target, and node_modules directories"
    )
    parser.add_argument(
        "-s", "--sort",
        action="store_true",
        help="Sort by node count (descending)"
    )
    parser.add_argument(
        "-a", "--all",
        action="store_true",
        help="Include the total for the parent directory"
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

    # Collect counts for each subdirectory
    results = []

    try:
        for entry in sorted(directory.iterdir()):
            if entry.is_dir():
                # Skip ignored directories at this level
                if entry.name in ignore_dirs:
                    continue

                file_count, dir_count, total_nodes = count_nodes(entry, ignore_dirs)
                results.append((total_nodes, entry.name, file_count, dir_count))
    except PermissionError:
        print(f"Error: Permission denied for {directory}")
        return 1

    # Sort if requested
    if args.sort:
        results.sort(reverse=True)

    # Display results
    if not results:
        print(f"No subdirectories found in {directory}")
        return 0

    # Find the maximum width for alignment
    max_count = max(r[0] for r in results) if results else 0
    count_width = len(f"{max_count:,}")

    for total_nodes, name, file_count, dir_count in results:
        print(f"{total_nodes:>{count_width},}  {name}")

    # Optionally show total for parent directory
    if args.all:
        file_count, dir_count, total_nodes = count_nodes(directory, ignore_dirs)
        print(f"{total_nodes:>{count_width},}  . (total)")

    return 0


if __name__ == "__main__":
    exit(main())
