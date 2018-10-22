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
            exec emacsclient -c -a mg
            ;;
        "file browser ")
            exec pcmanfm
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
            exec st -f 'Hack:size=10:antialias=true:autohint=true' -g 120x34
            ;;
        "top ")
            exec st -f 'Hack:size=10:antialias=true:autohint=true' -g 120x34 -e htop
            ;;
        "web browser")
            exec firefox
            ;;
    esac
}

command=$(echo -ne $COMMANDS | dmenu "$@")
[ -n "$command" ] && launch "$command"
