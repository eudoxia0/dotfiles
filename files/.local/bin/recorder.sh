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
    TIMESTAMP=$(date "+%Y/%m/%d/%H-%M-%S")

    # Create the directory structure
    DIR_PATH="$HOME/files/4 Resources/6 Images/logs/$TIMESTAMP"
    mkdir -p "$DIR_PATH"

    # Take a screenshot and save it to the specified path
    FILE_PATH="$DIR_PATH.png"
    scrot "$FILE_PATH"

    # Wait 60 seconds
    sleep 60
done
