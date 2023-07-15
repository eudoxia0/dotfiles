#!/usr/bin/env bash

picom  --backend glx --glx-fshader-win "$(cat /home/eudoxia/.local/share/eudoxia/monochrome.glsl)" --legacy-backends
