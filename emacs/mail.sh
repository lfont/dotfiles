#!/usr/bin/env bash

if ! tmux has -t mail; then
  tmux new -s mail -d 'emacs -nw -f my/mu4e'
fi

emacsclient -s mail -F '((name . "mail"))' -c
