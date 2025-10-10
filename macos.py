#!/usr/bin/env python3
"""
Apply macOS settings in a declarative way.
"""

import subprocess

REPLACEMENTS = {
    ":admin": "📝",
    ":alarm": "⏰",
    ":art": "🎨",
    ":ask": "💬",
    ":basket": "🧺",
    ":bed": "🛏️",
    ":bento": "🍱",
    ":bird": "🐦",
    ":block": "💹",
    ":blush": "😊",
    ":books": "📚",
    ":box": "📦",
    ":cal": "📆",
    ":cart": "🛒",
    ":chore": "🔧",
    ":coffee": "☕",
    ":cook": "🍳",
    ":dash": "💨",
    ":disc": "💿",
    ":down": "⬇️",
    ":drop": "💧",
    ":dry": "💨",
    ":env": "✉️",
    ":feat": "🌿",
    ":folder": "📁",
    ":heart": "❤️",
    ":iron": "♨️",
    ":laptop": "💻",
    ":log": "📝",
    ":milk": "🥛",
    ":muscle": "💪",
    ":phone": "☎️",
    ":pill": "💊",
    ":read": "📖",
    ":shake": "🥛",
    ":shirt": "👕",
    ":shop": "🛍️",
    ":shower": "🚿",
    ":soap": "🧼",
    ":steam": "♨️",
    ":talk": "💬",
    ":tea": "🍵",
    ":tp": "🧻",
    ":train": "🚄",
    ":tram": "🚊",
    ":trash": "🗑️",
    ":walk": "🚶‍♂️",
    ":wash": "💦",
    ":water": "💧",
    ":wolf": "🐺",
    ":write": "✍️",
}


def encode_apple_unicode(text: str) -> str:
    """Convert Unicode characters to Apple's \\Uxxxx format"""
    # Encode as UTF-16-BE (big-endian), then format each pair of bytes as \Uxxxx
    encoded: bytes = text.encode("utf-16-be")
    result: list[str] = []
    for i in range(0, len(encoded), 2):
        code_unit = int.from_bytes(encoded[i : i + 2], "big")
        result.append(f"\\U{code_unit:04x}")
    return "".join(result)


def main():
    cmd = ["defaults", "write", "-g", "NSUserDictionaryReplacementItems", "-array"]
    for replace, with_text in REPLACEMENTS.items():
        encoded = encode_apple_unicode(with_text)
        entry = f'{{ on = 1; replace = "{replace}"; with = "{encoded}"; }}'
        cmd.append(entry)
    _ = subprocess.run(cmd, check=True)


if __name__ == "__main__":
    main()
