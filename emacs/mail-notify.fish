#!/usr/bin/env fish

#
# -mmin -5: consider only messages that were created / changed in the
# the last 5 minutes
#
function notify
    set -l CHECKDIR $argv[1]

    for f in (find $CHECKDIR -mmin -5 -a -type f)
        set subject (mu view $f | grep '^Subject:' | sed 's/^Subject://')
        notify-send "Mail" $subject
    end
end

for d in $argv
    notify $d
end
