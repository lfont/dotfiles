#!/usr/bin/env bash

REMOTE_HOST="$1"
REMOTE_PATH=/media/seagate/Music
REMOTE_PLAYLISTS_PATH=/media/seagate/Playlists

LOCAL_PATH=~/Music/good
LOCAL_PLAYLISTS_PATH=~/Music/playlists
LOCAL_PLAYLISTS_TEMP_PATH=/tmp/playlists

function fix_perms() {
  find $LOCAL_PATH/ -type d -name ".*" -print0 | xargs -0 rm -rf
  find $LOCAL_PATH/ -type d -print0 | xargs -0 chmod 775
  find $LOCAL_PATH/ -type f -print0 | xargs -0 chmod 664
}

function download() {
  rsync -a --partial --progress $REMOTE_HOST:$REMOTE_PATH/ $LOCAL_PATH/
  fix_perms
}

if [ "$2" == "-d" ]
then
    download
else
  # sync music
  fix_perms
  rsync -a --partial --progress --delete --exclude="/Incoming/" $LOCAL_PATH/ $REMOTE_HOST:$REMOTE_PATH/

  # fix music paths in playlists
  mkdir -p "$LOCAL_PLAYLISTS_TEMP_PATH"
  rm -rf "$LOCAL_PLAYLISTS_TEMP_PATH/*"
  for playlist in "$LOCAL_PLAYLISTS_PATH"/*.m3u
  do
    cat "$playlist" | sed -e 's,^.*$,'"$REMOTE_PATH"'/&,' > "$LOCAL_PLAYLISTS_TEMP_PATH/$(basename $playlist)"
  done

  # sync playlists
  rsync -a --progress --delete "$LOCAL_PLAYLISTS_TEMP_PATH/" $REMOTE_HOST:"$REMOTE_PLAYLISTS_PATH/"

  # triggrer library update (require API >= 1.15.0)
  #SALT=$(tr -cd '[:alnum:]' < /dev/urandom | fold -w12 | head -n1)
  #TOKEN=$(pass show bibimbap/subsonic/$USER | head -n 1 | echo "$(cat -)$SALT" | md5sum | cut -d' ' -f1)
  #curl "https://subsonic.$REMOTE_HOST/rest/startScan?u=$USER&t=$TOKEN&s=$SALT&v=1.15.0&c=sync-music.sh"
fi
