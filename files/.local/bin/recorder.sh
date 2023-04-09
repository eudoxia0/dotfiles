#!/bin/bash

# Ensure scrot is installed
if ! command -v scrot &> /dev/null
then
    echo "scrot could not be found. Please install it and try again."
    exit 1
fi

# Main loop
while true; do
    # Get the current timestamp
    DATE=$(date "+%Y/%m/%d")
    TIME=$(date "+%H-%M-%S")

    # Create the directory structure down to the day
    DIR_PATH="$HOME/files/4 Resources/6 Images/logs/$DATE"
    mkdir -p "$DIR_PATH"

    # Take a screenshot and save it to the specified path with H-M-S.png format
    FILE_PATH="$DIR_PATH/$TIME.png"
    scrot "$FILE_PATH"

    # Wait 60 seconds
    sleep 60
done