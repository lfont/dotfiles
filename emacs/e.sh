#!/usr/bin/env bash

if [[ $EUID -ne 0 ]]; then
    exec emacsclient -a "" -nw "$@"
else
    exec emacs --eval "(setq make-backup-files nil); (setq auto-save-default nil)" -nw "$@"
fi
