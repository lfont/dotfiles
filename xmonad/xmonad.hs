-- http://www.nepherte.be/step-by-step-configuration-of-xmonad/

-- Import statements
import XMonad
import XMonad.Config.Desktop
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Layout.Minimize

import Control.Monad (liftM2)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- Define the names of all workspaces
myWorkspaces :: [String]
myWorkspaces = [ "1:www", "2:mail", "3:media", "4", "5" ]

-- Layout
myLayout = Full ||| tiledH ||| tiledV
    where
      tiledH = minimize (Tall 1 (3/100) (1/2))
      tiledV = Mirror tiledH

-- Windows management
myManageHook = composeAll . concat $
               [
                [ className =? c --> shiftFloat "5"      | c <- myClass5ShiftFloats ],
                [ className =? c --> viewShift "1:www"   | c <- myClassWwwShifts    ],
                [ className =? c --> viewShift "3:media" | c <- myClassMediaShifts  ],
                [ resource  =? r --> doIgnore            | r <- myResourceIgnores   ],
                [ resource  =? c --> doFloat             | c <- myResourceFloats    ],
                [ title     =? t --> viewShift "2:mail"  | t <- myTitleMailShifts   ]
               ]
    where
      viewShift           = doF . liftM2 (.) W.greedyView W.shift
      shiftFloat          = \w -> doFloat <+> doShift w
      myClass5ShiftFloats = [ "Google-chrome" ]
      myClassMediaShifts  = [ "Vlc", "Spotify", "Audacious", "Gimp" ]
      myClassWwwShifts    = [ "Firefox", "Chromium" ]
      myResourceFloats    = [ "xfce4-appfinder" ]
      myResourceIgnores   = [ "stalonetray" ]
      myTitleMailShifts   = [ "mail", "jabber" ]

-- Keys binding
myKeys (XConfig {modMask = modm}) = M.fromList $
    [
        -- Minimize window
        ((modm,                 xK_d),       withFocused minimizeWindow),
        ((modm .|. shiftMask,   xK_d),       sendMessage RestoreNextMinimizedWin),
        -- Rebind dmenu
        ((modm,                 xK_p),       spawn "dmenu-bind.sh"),
        -- Rebind gmrun
        ((modm .|. shiftMask,   xK_p),       spawn "xfce4-appfinder"),
        -- Lock the screen
        ((modm .|. controlMask, xK_l),       spawn "slock"),
        -- Web Browser
        ((modm .|. controlMask, xK_w),       spawn "$BROWSER"),
        -- Editor
        ((modm .|. controlMask, xK_e),       spawn "$VISUAL"),
        -- File Browser
        ((modm .|. controlMask, xK_f),       spawn "thunar"),
        -- mail
        ((modm .|. controlMask, xK_m),       spawn "emacs -T mail -f my/mu4e-start"),
        -- jabber
        ((modm .|. controlMask, xK_j),       spawn "emacs -T jabber -f my/jabber-start"),
        -- Audio volume
        ((0,                    0x1008FF13), spawn "audio-volume.sh up"),
        ((0,                    0x1008FF11), spawn "audio-volume.sh down")
    ]

-- Define terminal
myTerminal :: String
myTerminal = "emacsclient -c -e '(multi-term)' || emacs -f 'multi-term'"

-- Startup
myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawn     "xmonad-start-once.sh"

-- Panel
myLogHook h = dynamicLogWithPP $ prettyPrinter h

prettyPrinter h = xmobarPP
    {
      ppOutput = hPutStrLn h,
      ppTitle  = xmobarColor "cyan" "" . shorten 55
    }

-- Run XMonad
main :: IO ()
main = do
  xmobarPanel <- spawnPipe "xmobar"
  xmonad $ desktopConfig
       {
         manageHook  = myManageHook <+> manageHook desktopConfig,
         layoutHook  = desktopLayoutModifiers $ myLayout,
         logHook     = myLogHook xmobarPanel <+> logHook desktopConfig,
         modMask     = mod4Mask, -- Rebind mod to windows key
         keys        = myKeys <+> keys desktopConfig,
         terminal    = myTerminal,
         workspaces  = myWorkspaces,
         startupHook = startupHook desktopConfig >> myStartupHook
       }
