#!/usr/bin/env fish

if begin; test (pidof emacs); or test (pidof emacs24); end
   emacsclient -nw $argv
else
  emacs -nw $argv
end

exit $status

