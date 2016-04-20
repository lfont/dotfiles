# -*-sh-*-
. ~/.xprofile

xhost +SI:localuser:$USER

if command -v systemctl >/dev/null; then
  exec emacs --eval "(with-eval-after-load \"init-exwm\" (exwm-enable))"
else
  exec ck-launch-session emacs --eval "(with-eval-after-load \"init-exwm\" (exwm-enable))"
fi
