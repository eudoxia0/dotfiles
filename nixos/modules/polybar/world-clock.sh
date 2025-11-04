#!/usr/bin/env bash

# World clock script for polybar

SF=$(TZ="America/Los_Angeles" date +"%H:%M")
NYC=$(TZ="America/New_York" date +"%H:%M")
LDN=$(TZ="Europe/London" date +"%H:%M")
MNL=$(TZ="Asia/Manila" date +"%H:%M")

# Use polybar color codes: %{F#707880} for muted text, %{F-} to reset to default
echo "%{F#707880}SF%{F-} $SF %{F#707880}- NYC%{F-} $NYC %{F#707880}- LDN%{F-} $LDN %{F#707880}- MNL%{F-} $MNL"
