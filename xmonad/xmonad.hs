-- http://www.nepherte.be/step-by-step-configuration-of-xmonad/

-- Import statements
import XMonad

-- Define terminal
myTerminal = "urxvt"

-- Run XMonad
main = do
    xmonad $ defaultConfig {
       terminal = myTerminal
    }
