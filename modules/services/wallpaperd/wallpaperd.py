import os
import random
import subprocess
import time
from pathlib import Path

WALLPAPER_DIR: str = os.path.expanduser("~/.eudoxia.d/data/wallpaper")
INTERVAL: int = 300


def get_wallpapers(directory: str) -> list[Path]:
    wallpapers: list[Path] = []
    for root, _, files in os.walk(directory):
        for f in files:
            wallpapers.append(Path(os.path.join(root, f)))
    return wallpapers


def set_wallpaper(path: Path) -> None:
    subprocess.run(
        ["feh", "--no-fehbg", "--bg-fill", path],
        check=True,
    )


def main() -> None:
    while True:
        wallpapers: list[Path] = get_wallpapers(WALLPAPER_DIR)
        if wallpapers:
            choice: Path = random.choice(wallpapers)
            set_wallpaper(choice)
        time.sleep(INTERVAL)


if __name__ == "__main__":
    main()
