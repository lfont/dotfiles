#!/usr/bin/env bash

LOCAL_PATH="$HOME/Music"
LOCAL_PLAYLISTS="$LOCAL_PATH/playlists"
REMOTE_PATH="/media/$USER/$1"
REMOTE_PLAYLISTS="$REMOTE_PATH/.playlists"
REMOTE_PLAYLISTS_LOG="$REMOTE_PLAYLISTS.log"

if [ "$2" == "-u" ]
then
    # update playlists
    beet splupdate

    # generate recently added playlist
    beet list -p added:$(date +"%Y-%m-%d" --date="2 day ago").. | sed -e 's,'"$LOCAL_PATH"'/,,' > "$LOCAL_PLAYLISTS/_recently-added.m3u"
fi

# list of tracks to sync
cat $(cat "$REMOTE_PLAYLISTS" | sed -e 's,^.*$,'"$LOCAL_PLAYLISTS"'/&.m3u,' | tr '\n' ' ') | sort -V | uniq > "$REMOTE_PLAYLISTS_LOG"

# copy playlists
cat "$REMOTE_PLAYLISTS_LOG" | sed -e 's,^.*$,Music/&,' > "$REMOTE_PATH/_music.m3u"
while read playlist
do
  cat "$LOCAL_PLAYLISTS/$playlist.m3u" | sed -e 's,^.*$,Music/&,' > "$REMOTE_PATH/$playlist.m3u"
done < "$REMOTE_PLAYLISTS"

# append list of covers to sync
echo -e "$(cat "$REMOTE_PLAYLISTS_LOG")\n$(cat "$REMOTE_PLAYLISTS_LOG" | cut -d/ -f-2 | sort -V | uniq | sed -e 's,^.*$,&/cover.jpg,')" | sort -V > "$REMOTE_PLAYLISTS_LOG.tmp"
mv "$REMOTE_PLAYLISTS_LOG.tmp" "$REMOTE_PLAYLISTS_LOG"

# delete old files (--delete does not work with --files-from)
diff <(cat "$REMOTE_PLAYLISTS_LOG") <(find "$REMOTE_PATH/Music/" -type f | sed -e 's,'"$REMOTE_PATH"'/Music/,,' | sort -V) | grep -e '^>' | cut -d' ' -f2- | while read file
do
  echo "deleting: $file"
  rm "$REMOTE_PATH/Music/$file"
done
find "$REMOTE_PATH/Music/" -empty -delete

# sync
rsync -a --progress --modify-window=1 --files-from="$REMOTE_PLAYLISTS_LOG" "$LOCAL_PATH/" "$REMOTE_PATH/Music/"
df -h "$REMOTE_PATH"
