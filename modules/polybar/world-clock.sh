#!/usr/bin/env bash

# World clock script for polybar

MNL=$(TZ="Asia/Manila" date +"%H:%M")
SF=$(TZ="America/Los_Angeles" date +"%H:%M")
NYC=$(TZ="America/New_York" date +"%H:%M")
MVD=$(TZ="America/Montevideo" date +"%H:%M")
LDN=$(TZ="Europe/London" date +"%H:%M")

# Use polybar color codes: %{F#707880} for muted text, %{F-} to reset to default
echo -n "%{F#707880}MNL%{F-} $MNL  "
echo -n "%{F#707880}SF%{F-} $SF  "
echo -n "%{F#707880}NYC%{F-} $NYC  "
echo -n "%{F#707880}MVD%{F-} $MVD  "
echo    "%{F#707880}LDN%{F-} $LDN"
