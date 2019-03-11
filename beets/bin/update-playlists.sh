#!/usr/bin/env bash

LOCAL_PATH=~/Music
LOCAL_PLAYLISTS_PATH=~/Music/Playlists
LOCAL_PLAYLISTS_TEMP_PATH=/tmp/playlists

echo "------------------------------------ UPDATE PLAYLISTS"
beet splupdate

# generate recently added playlist
beet list -p added:$(date +"%Y-%m-%d" --date="2 day ago").. | sed -e 's,'"$LOCAL_PATH"'/,../,' > "$LOCAL_PLAYLISTS_PATH/_recently-added.m3u"
