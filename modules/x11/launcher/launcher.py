#!/usr/bin/env python
from subprocess import CompletedProcess, Popen, run

OPTIONS: list[tuple[str, list[str]]] = [
    ("1password", ["1password"]),
    ("baobab", ["baobab"]),
    ("calibre", ["calibre-scaled"]),
    ("chromium", ["chromium-browser"]),
    ("emacs", ["emacs"]),
    ("firefox", ["firefox"]),
    ("folder: dotfiles", ["zeditor", "/home/eudoxia/root/1-workspace/dotfiles"]),
    ("folder: flashcards", ["zeditor", "/home/eudoxia/root/1-workspace/flashcards"]),
    ("font manager", ["font-manager"]),
    ("heroic", ["heroic"]),
    ("libreoffice", ["libreoffice"]),
    ("lock", ["xscreensaver-command", "-lock"]),
    ("music player", ["strawberry"]),
    ("obsidian", ["obsidian"]),
    ("pavucontrol", ["pavucontrol"]),
    ("screenshot / desktop", ["scrot", "--freeze"]),
    ("screenshot / region", ["scrot", "--freeze", "--select"]),
    ("screenshot / window", ["scrot", "--freeze", "--focused"]),
    ("signal", ["signal-desktop"]),
    ("sound settings", ["pavucontrol"]),
    ("strawberry", ["strawberry"]),
    ("todoist", ["todoist"]),
    ("transmission", ["transmission-gtk"]),
    ("zed", ["zeditor"]),
]

NAMES: list[str] = sorted([n for n, _ in OPTIONS])


def main():
    choices: str = "\n".join(NAMES)
    result: CompletedProcess[str] = run(
        ["rofi", "-i", "-dmenu"],
        input=choices,
        capture_output=True,
        text=True,
    )
    choice: str = result.stdout.strip()
    for name, cmd in OPTIONS:
        if choice == name:
            _ = Popen(cmd, start_new_session=True)
            break


if __name__ == "__main__":
    main()
