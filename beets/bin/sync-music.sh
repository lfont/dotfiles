#!/usr/bin/env bash

REMOTE_HOST="$1"
LOCAL_PATH=~/Music/good
REMOTE_PATH=/media/seagate/Music

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
  fix_perms
  rsync -a --partial --progress --delete --exclude="/Incoming/" $LOCAL_PATH/ $REMOTE_HOST:$REMOTE_PATH/
fi
