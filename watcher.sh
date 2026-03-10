#!/bin/bash

# Überprüfen, ob ein Pfad übergeben wurde
if [ -z "$1" ]; then
    echo "Usage: $0 <path-to-file>"
    exit 1
fi

FILE="$1"

# Überprüfen, ob die Datei existiert
if [ ! -e "$FILE" ]; then
    echo "Fehler: Datei '$FILE' existiert nicht."
    exit 1
fi

echo "Watching $FILE ..."

fswatch -o "$FILE" | while read _; do
    clear
    target/release/rux build my_project
done
