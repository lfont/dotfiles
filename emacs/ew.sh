#!/usr/bin/env bash

emacsclient -c "$@" || emacs "$@"
