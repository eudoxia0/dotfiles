#!/usr/bin/env bash

function probe_battery() {
    local var charge=`acpi -b | grep -oP "\d+%" | tr -d "%"`
    if (( charge < 10 )); then
	xmessage -center "Battery level: $charge%"
    fi
    if (( charge > 90 )); then
	xmessage -center "Battery level: $charge%"
    fi
}

while true; do
    probe_battery;
    sleep 300
done
