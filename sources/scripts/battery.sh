#!/usr/bin/env bash

output=$(acpi 2>&1)

if [[ "$output" =~ "No support for device type: power_supply" ]]; then
  exit 0
fi

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
    sleep 15
done
