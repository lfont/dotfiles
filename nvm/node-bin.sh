#!/usr/bin/env bash

BIN="$0"

for v in $(ls -r ~/.nvm/versions/node); do
    if [[ -x ~/.nvm/versions/node/$v/bin/$BIN ]]; then
        exec ~/.nvm/versions/node/$v/bin/$BIN "$@"
    fi
done
