#!/usr/bin/env bash

# World clock script for polybar
# Shows time in Manila, San Francisco, NYC, and London

MNL=$(TZ="Asia/Manila" date +"%H:%M")
SF=$(TZ="America/Los_Angeles" date +"%H:%M")
NYC=$(TZ="America/New_York" date +"%H:%M")
LDN=$(TZ="Europe/London" date +"%H:%M")

echo "MNL $MNL | SF $SF | NYC $NYC | LDN $LDN"
