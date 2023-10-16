#!/bin/bash

not_sourced() {
    >&2 echo "ERROR: Script should be sourced"
    exit 1
}

(return 0 2>/dev/null) || not_sourced


numlist="$(python gen10rand.py)"
zenity --info --title="PAXY EXECUTION" --text="USING DELAYS LIST: $numlist" &
erl -eval "paxy:start($numlist)"
