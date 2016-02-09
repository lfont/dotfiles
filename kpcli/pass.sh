#!/usr/bin/env bash

if tmux has -t pass; then
    urxvt -T pass -e tmux attach -t pass
else
    urxvt -T pass -e tmux new -s pass '~/bin/kpcli.pl --kdb ~/AppData/KeePassX/default.kdbx'
fi
