#!/usr/bin/env bash

#
# -mmin -5: consider only messages that were created / changed in the
# the last 5 minutes
#
function notify() {
    CHECKDIR="$1"

    for f in $(find "$CHECKDIR" -mmin -5 -a -type f); do
        subject=$(mu view "$f" | grep "^Subject:" | sed "s/^Subject://")
        notify-send "Mail" "$subject"
        sleep 1
    done
}

for d in $@; do
    notify "$d"
done
