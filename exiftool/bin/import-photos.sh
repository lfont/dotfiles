#!/usr/bin/env bash

SRC="${1:-.}/"
DEST=~/Pictures/Photos
TARGET=filename

if [ "$2" == "-n" ]; then
    TARGET=testname
fi

exiftool -r -d "$DEST"/%Y/%Y%m%d_%H%M%S%%-c.%%e \
         "-${TARGET}<filemodifydate" "-${TARGET}<createdate" "-${TARGET}<datetimeoriginal" \
         "$SRC"
