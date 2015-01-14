#!/usr/bin/env bash
exe=`dmenu_run -l 5 -nb '#151617' -nf '#d8d8d8' -sb '#d8d8d8' -sf '#151617' -fn 'Inconsolata-11:Regular'` && eval "exec $exe"
