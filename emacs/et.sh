#!/usr/bin/env bash

emacsclient -c -e "(multi-term)" || emacs -f "multi-term"
