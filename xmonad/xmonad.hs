-- http://www.nepherte.be/step-by-step-configuration-of-xmonad/

-- Import statements
import XMonad
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import Control.Monad (liftM2)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import System.IO

-- Define the names of all workspaces
myWorkspaces = [ "1:web", "2:chat", "3:browse", "4:media", "5:other" ]

-- Layout
myLayout = avoidStruts $ Full ||| tiled ||| Mirror tiled
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- Windows management
myManageHook = composeAll . concat $
    [
        -- Applications that go to web
        [ className =? b --> viewShift "1:web"   | b  <- myClassWebShifts   ],
        -- Applications that go to chat
        [ title     =? c --> viewShift "2:chat"  | c <- myTitleChatShifts   ],
        -- Applications that go to media
        [ className =? d --> viewShift "4:media" | d  <- myClassMediaShifts ],
        -- Applications to ignore
        [ resource  =? i --> doIgnore            | i  <- myResourceIgnores  ]
    ]
    where
      viewShift          = doF . liftM2 (.) W.greedyView W.shift
      myClassWebShifts   = [ "Firefox", "Chromium" ]
      myTitleChatShifts  = [ "mail", "jabber" ]
      myClassMediaShifts = [ "Vlc", "Spotify" ]
      myResourceIgnores  = [ "stalonetray" ]

-- Define keys to add
keysToAdd x =
    [
        -- Rebind dmenu
        ((modMask x, xK_p), spawn "~/.config/dmenu/dmenu-bind.sh"),
        -- Lock the screen
        (((modMask x .|. controlMask), xK_l), spawn "xscreensaver-command -lock"),
        -- File Browser
        (((modMask x .|. controlMask), xK_f), spawn "pcmanfm"),
        -- Web Browser
        (((modMask x .|. controlMask), xK_w), spawn "firefox"),
        -- gnus
        (((modMask x .|. controlMask), xK_m), spawn "emacs -T mail -f my/mu4e-start"),
        -- jabber
        (((modMask x .|. controlMask), xK_j), spawn "emacs -T jabber -f my/jabber-start"),
        -- Audio volume
        ((0, 0x1008FF13), spawn "audio-volume up"),
        ((0, 0x1008FF11), spawn "audio-volume down")
    ]

-- Define keys to remove
keysToRemove x =
    [
        -- Unused dmenu default binding
        (modMask x, xK_p),
        -- Unused gmrun binding
        (modMask x .|. shiftMask, xK_p)
    ]

-- Delete the keys combinations we want to remove.
strippedKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)

-- Compose all my key combinations
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))

-- Define terminal
myTerminal = "urxvt"

-- Startup
myStartupHook = do
    setWMName "LG3D"
    spawn "x11-autostart"

-- Workspace bar
myLogHook h = dynamicLogWithPP $ myPrettyPrinter h

myPrettyPrinter h = xmobarPP
    {
        ppOutput = hPutStrLn h,
        ppTitle = xmobarColor "cyan" "" . shorten 55
    }

-- Run XMonad
main = do
    workspaceBar <- spawnPipe "xmobar"

    xmonad $ defaultConfig
        {
            manageHook = manageDocks <+> myManageHook,
            layoutHook = myLayout,
            logHook = myLogHook workspaceBar,
            modMask = mod4Mask, -- Rebind mod to windows key
            keys = myKeys,
            terminal = myTerminal,
            workspaces = myWorkspaces,
            startupHook = myStartupHook
        }
