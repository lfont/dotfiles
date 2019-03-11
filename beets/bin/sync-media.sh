#!/usr/bin/env bash

LOCAL_PATH="$HOME/Music"

REMOTE_PATH="/media/$USER/$1"
REMOTE_PLAYLISTS_PATH="$REMOTE_PATH/.playlists"
REMOTE_PLAYLISTS_LOG_PATH="$REMOTE_PLAYLISTS_PATH.log"

if [ "$2" == "-u" ]
then
    update-playlists.sh
fi



echo "------------------------------------ COMPUTING FILES TO SYNC"
# list of tracks to sync
cat $(cat "$REMOTE_PLAYLISTS_PATH" | sed -e 's,^.*$,'"$LOCAL_PATH"'/Playlists/&,' | tr '\n' ' ') \
    | sort -V \
    | uniq \
    | sed -e 's,^../Library/,,' \
    > "$REMOTE_PLAYLISTS_LOG_PATH"

# copy playlists
while read playlist
do
    cat "$LOCAL_PATH/Playlists/$playlist" \
        | sed -e 's,^../Library/,../Music/,' \
        > "$REMOTE_PATH/Playlists/$playlist"
done < "$REMOTE_PLAYLISTS_PATH"

# append list of covers to sync
echo -e "$(cat "$REMOTE_PLAYLISTS_LOG_PATH")\n$(cat "$REMOTE_PLAYLISTS_LOG_PATH" | cut -d/ -f-2 | sort -V | uniq | sed -e 's,^.*$,&/cover.jpg,')" | sort -V > "$REMOTE_PLAYLISTS_LOG_PATH.tmp"
mv "$REMOTE_PLAYLISTS_LOG_PATH.tmp" "$REMOTE_PLAYLISTS_LOG_PATH"



echo "------------------------------------ DELETING UNNEEDED FILES"
# delete old files (--delete does not work with --files-from)
diff <(cat "$REMOTE_PLAYLISTS_LOG_PATH") <(find "$REMOTE_PATH/Music/" -type f | sed -e 's,'"$REMOTE_PATH"'/Music/,,' | sort -V) | grep -e '^>' | cut -d' ' -f2- | while read file
do
  echo "deleting: $file"
  rm "$REMOTE_PATH/Music/$file"
done
find "$REMOTE_PATH/Music/" -empty -delete



echo "------------------------------------ SYNC MUSIC"
rsync -a --progress --modify-window=1 --files-from="$REMOTE_PLAYLISTS_LOG_PATH" "$LOCAL_PATH/Library/" "$REMOTE_PATH/Music/"



echo "------------------------------------ FREE SPACE"
df -h "$REMOTE_PATH"
