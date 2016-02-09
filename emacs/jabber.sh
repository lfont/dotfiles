#!/usr/bin/env bash

if ! tmux has -t jabber; then
  tmux new -s jabber -d 'emacs -nw -f my/jabber'
fi

emacsclient -s jabber -F '((name . "jabber"))' -c
