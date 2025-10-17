#!/usr/bin/env python3
import subprocess
import re
import sys

output: str = subprocess.check_output(["smartctl", "-a", "disk7s1"], text=True)
match = re.search(r"Temperature:\s+(\d+)", output)
if match is not None:
    print(match.group(1))
    sys.exit(0)
else:
    print("Temperature not found")
    sys.exit(1)
