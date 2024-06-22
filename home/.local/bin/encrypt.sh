#!/bin/sh

if [ $# -ne 2 ]; then
    echo "Error: This script requires exactly two arguments."
    echo "Usage: encrypt.sh <input> <output>"
    exit 1
fi

if [ ! -f "$1" ]; then
    echo "Input file does not exist."
    exit 1
fi

if [ -e "$2" ]; then
    echo "Output file exists."
    exit 1
fi

age -p $1 > $2
