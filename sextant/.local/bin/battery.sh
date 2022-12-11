#!/usr/bin/env bash

function probe_battery() {
    local var charge=`acpi -b | grep -oP "\d+%" | tr -d "%"`
    if (( charge < 10 )); then
        notify-send "Plug Charger" "Battery level is $charge%"
    fi
    if (( charge > 90 )); then
	    notify-send "Unplug Charger" "Battery level is $charge%"
    fi
}

while true; do
    probe_battery;
    sleep 300
done
