#!/bin/bash

PROGRAM_DIR="../ntua-examples"

if [ ! -d "$PROGRAM_DIR" ]; then
    echo "Error: Directory '$PROGRAM_DIR' not found."
    exit 1
fi

for program in "$PROGRAM_DIR"/*.alan; do
    # Ensure the file exists
    if [ ! -f "$program" ]; then
        echo "No .alan files found in directory."
        exit 1
    fi

    echo "Testing program: $program"
    output=$(./alanc "$program" 2>&1)

    if echo "$output" | grep -q "Compilation succeeded"; then
        echo "Test passed for $program."
    else
        echo "Test failed for $program."
        echo "Output:"
        echo "$output"
    fi
    echo "------------------------------------------"
done
