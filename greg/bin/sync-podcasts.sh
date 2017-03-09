#!/usr/bin/env bash

MUSIC="$HOME/Music"
LOCAL_PATH="$MUSIC/podcasts"
REMOTE_PATH="/run/media/$USER/$1"
REMOTE_PODCASTS="$REMOTE_PATH/.podcasts"
REMOTE_PODCASTS_LOG="$REMOTE_PODCASTS.log"

if [ "$2" == "-u" ]
then
    # sync new podcasts
    greg sync
fi

# list of podcasts to sync
echo -n > "$REMOTE_PODCASTS_LOG"
cat "$REMOTE_PODCASTS" | while read podcast
do
  echo "$podcast" | sed -e 's,^.*$,'"$LOCAL_PATH"'/"&",' | xargs ls -Art | tail -n 3 | sed -e 's,^.*$,'"$podcast"'/&,' | sort >> "$REMOTE_PODCASTS_LOG"
done

# copy playlist
cat "$REMOTE_PODCASTS_LOG" | sed -e 's,^.*$,Podcasts/&,' > "$REMOTE_PATH/_podcasts.m3u"

# delete old files (--delete does not work with --files-from)
diff <(cat "$REMOTE_PODCASTS_LOG") <(find "$REMOTE_PATH/Podcasts/" -type f | sed -e 's,'"$REMOTE_PATH"'/Podcasts/,,' | sort) | grep -e '^>' | cut -d' ' -f2- | while read file
do
  echo "deleting: $file"
  rm "$REMOTE_PATH/Podcasts/$file"
done
find "$REMOTE_PATH/Podcasts/" -empty -delete

# sync
rsync -a --progress --modify-window=1 --files-from="$REMOTE_PODCASTS_LOG" "$LOCAL_PATH/" "$REMOTE_PATH/Podcasts/"
