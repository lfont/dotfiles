#!/usr/bin/env bash

LOCAL_PATH=~/Music/Library
LOCAL_PLAYLISTS_PATH=~/Music/Playlists
LOCAL_PLAYLISTS_TEMP_PATH=/tmp/playlists

REMOTE_HOST="$1"
REMOTE_PATH=/mnt/seagate/Music/Library
REMOTE_PLAYLISTS_PATH=/mnt/seagate/Music/Playlists

function fix_perms() {
  find $LOCAL_PATH/ -type d -name ".*" -print0 | xargs -0 rm -rf
  find $LOCAL_PATH/ -type d -print0 | xargs -0 chmod 775
  find $LOCAL_PATH/ -type f -print0 | xargs -0 chmod 664
}

if [ "$2" == "-d" ]
then
  echo "------------------------------------ PULL MUSIC"
  rsync -a --partial --progress --exclude="/playlists/" $REMOTE_HOST:$REMOTE_PATH/ $LOCAL_PATH/
  fix_perms
else
  echo "------------------------------------ PUSH MUSIC"
  fix_perms
  rsync -a --partial --progress --exclude="/Incoming/" --exclude="/playlists/" $LOCAL_PATH/ $REMOTE_HOST:$REMOTE_PATH/

  if [ "$2" == "-u" ]
  then
      update-playlists.sh
  fi

  echo "------------------------------------ PUSH PLAYLISTS"
  # fix music paths in playlists
  mkdir -p "$LOCAL_PLAYLISTS_TEMP_PATH"
  rm -rf "$LOCAL_PLAYLISTS_TEMP_PATH"/*
  for playlist in "$LOCAL_PLAYLISTS_PATH"/*.m3u
  do
    cat "$playlist" | sed -e 's,^.*$,'"$REMOTE_PATH"'/&,' > "$LOCAL_PLAYLISTS_TEMP_PATH/$(basename $playlist)"
  done

  # sync
  rsync -a --progress --checksum "$LOCAL_PLAYLISTS_TEMP_PATH/" $REMOTE_HOST:"$REMOTE_PLAYLISTS_PATH/"

  echo "------------------------------------ UPDATE DATABASE"
  SALT=$(tr -cd '[:alnum:]' < /dev/urandom | fold -w6 | head -n1 | tr '[:upper:]' '[:lower:]')
  TOKEN=$(pass show bibimbap/subsonic/$USER | head -n 1 | echo -n "$(cat -)$SALT" | md5sum | cut -d' ' -f1)
  curl "https://airsonic.$REMOTE_HOST/rest/startScan?u=$USER&t=$TOKEN&s=$SALT&v=1.15.0&c=sync-music.sh"
fi
