#!/usr/bin/env bash

LOCAL_PATH=~/Music
LOCAL_PLAYLISTS_PATH=~/Music/playlists
LOCAL_PLAYLISTS_TEMP_PATH=/tmp/playlists

REMOTE_HOST="$1"
REMOTE_PATH=/mnt/seagate/Music
REMOTE_PLAYLISTS_PATH=/mnt/seagate/Music/playlists

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
  rsync -a --partial --progress --delete --exclude="/Incoming/" --exclude="/playlists/" $LOCAL_PATH/ $REMOTE_HOST:$REMOTE_PATH/

  if [ "$2" == "-u" ]
  then
    echo "------------------------------------ UPDATE PLAYLISTS"
    beet splupdate

    # generate recently added playlist
    beet list -p added:$(date +"%Y-%m-%d" --date="2 day ago").. | sed -e 's,'"$LOCAL_PATH"'/,,' > "$LOCAL_PLAYLISTS_PATH/_recently-added.m3u"
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
  rsync -a --progress --delete --checksum "$LOCAL_PLAYLISTS_TEMP_PATH/" $REMOTE_HOST:"$REMOTE_PLAYLISTS_PATH/"

  echo "------------------------------------ UPDATE DATABASE"
  SALT=$(tr -cd '[:alnum:]' < /dev/urandom | fold -w6 | head -n1 | tr '[:upper:]' '[:lower:]')
  TOKEN=$(pass show bibimbap/subsonic/$USER | head -n 1 | echo -n "$(cat -)$SALT" | md5sum | cut -d' ' -f1)
  curl "https://airsonic.$REMOTE_HOST/rest/startScan?u=$USER&t=$TOKEN&s=$SALT&v=1.15.0&c=sync-music.sh"
fi
