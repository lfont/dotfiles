#!/usr/bin/env fish

if test (pidof emacs24)
   emacsclient -nw $argv
else
  emacs -nw $argv
end

exit $status

