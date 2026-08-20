#!/usr/bin/env python
"""
Rename files from:

    image_<Unix time in ms>.<ext>

format to this format:

    YYYY-MM-DD-HHMMSS.<ext>

"""

import argparse
import os
import re
import sys
from datetime import datetime

PATTERN = re.compile(r"^image_(\d+)\.([^.]+)$")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-n",
        "--dry-run",
        action="store_true",
        help="Show what would be renamed without doing it.",
    )
    args = parser.parse_args()

    renames = {}
    for name in os.listdir("."):
        if not os.path.isfile(name):
            continue
        m = PATTERN.match(name)
        if not m:
            continue
        ms, ext = m.group(1), m.group(2)
        ts = int(ms) / 1000.0
        dt = datetime.fromtimestamp(ts)
        new_name = f"{dt.strftime('%Y-%m-%d-%H%M%S')}.{ext}"
        renames.setdefault(new_name, []).append(name)

    for new_name, sources in renames.items():
        for i, src in enumerate(sorted(sources)):
            target = new_name
            if len(sources) > 1:
                base, ext = os.path.splitext(new_name)
                target = f"{base}_{i + 1}{ext}" if i > 0 else new_name
            if os.path.exists(target) and target != src:
                print(f"SKIP (exists): {src} -> {target}", file=sys.stderr)
                continue
            if args.dry_run:
                print(f"[dry-run] {src} -> {target}")
            else:
                os.rename(src, target)
                print(f"{src} -> {target}")


if __name__ == "__main__":
    main()
