#!/usr/bin/env bash

csfl="/tmp/.conky-xmobar.out"

>$csfl

conky -qc ~/dotfiles/conky/conkyrc-xmobar | while read line; do
    echo -n "$line" > $csfl
done
