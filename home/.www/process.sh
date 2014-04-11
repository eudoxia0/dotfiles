#!/bin/bash

# The default JSON is horrifying. Clean it up.
cat bookmarks.json | python -m json.tool > bookmarks.1.json
# Call the Python parser
python process-bookmarks.py > raw-bookmarks.yaml
# Clean up
rm bookmarks.1.json
