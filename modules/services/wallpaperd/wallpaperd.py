#!/usr/bin/env python3
import os
import random
import subprocess
import time

WALLPAPER_DIR: str = os.path.expanduser("~/.eudoxia.d/data/wallpaper")
INTERVAL: int = 300
EXTENSIONS: tuple[str, ...] = (".jpg", ".png")


def get_wallpapers(directory: str) -> list[str]:
    wallpapers = []
    for root, _, files in os.walk(directory):
        for f in files:
            if f.lower().endswith(EXTENSIONS):
                wallpapers.append(os.path.join(root, f))
    return wallpapers


def set_wallpaper(path: str) -> None:
    subprocess.run(
        ["feh", "--no-fehbg", "--bg-fill", path],
        check=True,
    )


def main() -> None:
    while True:
        wallpapers = get_wallpapers(WALLPAPER_DIR)
        if wallpapers:
            choice = random.choice(wallpapers)
            set_wallpaper(choice)
        time.sleep(INTERVAL)


if __name__ == "__main__":
    main()
