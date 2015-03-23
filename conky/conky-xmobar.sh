#!/usr/bin/env bash

csfl="/tmp/.inxmobar"

>$csfl

conky -qc ~/dotfiles/conky/conkyrc-xmobar | while read line; do
    echo -n "$line" > $csfl
done
