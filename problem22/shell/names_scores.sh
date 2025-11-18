#!/usr/bin/env sh

# Project Euler Problem 22: Names Scores
#
# WHY: Calculate total of all name scores from alphabetically sorted names
# EXPECTED: Correct sorting, alphabetical value calculation, and position-based scoring

# Find names.txt - search current directory then upwards
find_names_file() {
    dir="$PWD"
    while [ "$dir" != "/" ]; do
        if [ -f "$dir/names.txt" ]; then
            echo "$dir/names.txt"
            return 0
        fi
        dir=$(dirname "$dir")
    done
    echo "Error: names.txt not found" >&2
    return 1
}

NAMES_FILE=$(find_names_file) || exit 1

tr ',' '\n' < "$NAMES_FILE" | sort | awk '{value=0; for(i=1;i<=length($0);i++){char=toupper(substr($0,i,1)); value+=index("ABCDEFGHIJKLMNOPQRSTUVWXYZ",char)} total+=value*NR} END{print total}'
