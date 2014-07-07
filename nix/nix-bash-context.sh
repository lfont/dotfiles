#!/usr/bin/env bash

CMD=$(basename $0)
ARGS=$@

if ! [[ $CMD == nix-* ]]
then
    echo "$CMD is not a nix command"
    exit 1
fi

. ~/.nix-profile/etc/profile.d/nix.sh

$CMD $ARGS

