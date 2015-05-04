#!/usr/bin/env bash

set -e

CMD=$(basename $0)

case $CMD in
    halt)
        dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop
        ;;
    reboot)
        dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart
        ;;
    zzz)
        dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Suspend && slock
        ;;
    ZZZ)
        dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Hibernate && slock
        ;;
    *)
        echo 'unknow command'
        exit 1
esac

exit 0
