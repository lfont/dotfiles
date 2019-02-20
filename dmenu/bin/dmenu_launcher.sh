#!/usr/bin/env bash

COMMANDS="editor
\nfile browser
\nkeymap - fr swapcaps
\nkeymap - fr
\nkeymap - us intl swapcaps
\nkeymap - us intl
\nlock screen
\nmonitor - enable external
\nmonitor - disable external
\nterminal
\ntop
\nweb browser"

function launch () {
    case $1 in
        "editor ")
            eval exec "$VISUAL"
            ;;
        "file browser ")
            eval exec "$FILE_BROWSER"
            ;;
        "keymap - fr swapcaps ")
            exec setxkbmap -layout fr -option ctrl:swapcaps
            ;;
        "keymap - fr ")
            exec setxkbmap -layout fr -option ''
            ;;
        "keymap - us intl swapcaps ")
            exec setxkbmap -layout us -variant intl -option ctrl:swapcaps
            ;;
        "keymap - us intl ")
            exec setxkbmap -layout us -variant intl -option ''
            ;;
        "lock screen ")
            exec slock
            ;;
        "monitor - enable external ")
            exec ~/.screenlayout/2.sh
            ;;
        "monitor - disable external ")
            exec ~/.screenlayout/1.sh
            ;;
        "terminal ")
            eval exec "$TERMINAL"
            ;;
        "top ")
            eval exec "$TERMINAL" -e htop
            ;;
        "web browser")
            eval exec "$BROWSER"
            ;;
    esac
}

command=$(echo -ne $COMMANDS | dmenu "$@")
[ -n "$command" ] && launch "$command"
