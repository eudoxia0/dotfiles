#!/usr/bin/env python
import subprocess

APPS = {
    "1Password": ["1password"],
    "Baobab": ["baobab"],
    "Calibre": ["calibre"],
    "Chromium": ["chromium-browser"],
    "Fonts": ["font-manager"],
    "Heroic": ["heroic"],
    "LibreOffice": ["libreoffice"],
    "Lock": ["xscreensaver-command", "-lock"],
    "Music Player": ["strawberry"],
    "Obsidian": ["obsidian"],
    "Screenshot": ["scrot", "-f", "-s"],
    "Signal": ["signal-desktop"],
    "Sound Settings": ["pavucontrol"],
    "Todoist": ["todoist-x11"],
    "Transmission": ["transmission-gtk"],
    "Zed": ["zeditor"],
}


def main():
    choices = "\n".join(sorted(APPS.keys()))
    result = subprocess.run(
        ["rofi", "-i", "-dmenu"],
        input=choices,
        capture_output=True,
        text=True,
    )
    choice = result.stdout.strip()
    if choice in APPS:
        subprocess.Popen(APPS[choice], start_new_session=True)


if __name__ == "__main__":
    main()
