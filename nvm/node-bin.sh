#!/usr/bin/env bash

BIN=$(basename "$0")

for v in $(ls -r ~/.nvm/versions/node); do
    BIN_PATH=~/.nvm/versions/node/$v/bin/$BIN
    if [ -x $BIN_PATH ]; then
        exec $BIN_PATH "$@"
    fi
done
