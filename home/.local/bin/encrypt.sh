#!/bin/sh

if [ $# -ne 2 ]; then
    echo "Error: This script requires exactly two arguments."
    echo "Usage: encrypt.sh <input> <output>"
    exit 1
fi

input=$1
output=$2

if [ ! -f "$input" ]; then
    echo "Input file does not exist."
    exit 1
fi

if [ -e "$output" ]; then
    echo "Output file exists: $output"
    exit 1
fi

age -p -o $output $input
