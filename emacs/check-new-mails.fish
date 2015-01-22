#!/usr/bin/env fish

# put the path to your Inbox folder here
set CHECKDIR "$HOME/Maildir/fastmail/INBOX"

#
# -mmin -5: consider only messages that were created / changed in the
# the last 5 minutes
#
for f in (find $CHECKDIR -mmin -5 -a -type f)
    set subject (mu view $f | grep '^Subject:' | sed 's/^Subject://')
    notify-send "Mail" $subject
end
