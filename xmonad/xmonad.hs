-- Import statements
import XMonad
import XMonad.Config.Desktop
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Layout.Minimize
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W

import qualified Data.Map as M
import Data.Monoid

import Control.Monad (liftM2)

import Graphics.X11.ExtraTypes.XF86

import System.IO

-- Define the names of all workspaces
myWorkspaces :: [String]
myWorkspaces = [ "1", "2", "3:www", "4:mail", "5:media" ]

-- Layout
myLayout = Full ||| tiledH ||| tiledV
    where
      tiledH = spacing 2 $ minimize (Tall 1 (3/100) (1/2))
      tiledV = Mirror tiledH

-- Windows management
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll . concat $
               [
                [ className =? c --> shiftFloat "3:www"   | c <- myClassWwwShiftFloats ],
                [ className =? c --> viewShift  "3:www"   | c <- myClassWwwShifts      ],
                [ className =? c --> viewShift  "5:media" | c <- myClassMediaShifts    ],
                [ resource  =? r --> doIgnore             | r <- myResourceIgnores     ],
                [ resource  =? c --> doFloat              | c <- myResourceFloats      ],
                [ title     =? t --> viewShift  "4:mail"  | t <- myTitleMailShifts     ]
               ]
    where
      viewShift             = doF . liftM2 (.) W.greedyView W.shift
      shiftFloat            = \w -> doFloat <+> doShift w
      myClassMediaShifts    = [ "Vlc", "Spotify", "Audacious", "Gimp" ]
      myClassWwwShifts      = [ "Firefox", "Chromium" ]
      myClassWwwShiftFloats = [ "Google-chrome" ]
      myResourceFloats      = [ "xfce4-appfinder" ]
      myResourceIgnores     = [ "stalonetray" ]
      myTitleMailShifts     = [ "mail", "jabber" ]

-- Keys binding
menuXPConfig :: XPConfig
menuXPConfig = greenXPConfig
    {
      font         = "xft:Terminus-12:Regular",
      height       = 20,
      position     = Top,
      promptKeymap = emacsLikeXPKeymap
    }

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {modMask = modm}) = M.fromList $
    [
        -- Reload configuration
        ((modm,                 xK_q),                    spawn "xmonad --recompile && xmonad --restart"),
        -- Minimize window
        ((modm,                 xK_d),                    withFocused minimizeWindow),
        ((modm .|. shiftMask,   xK_d),                    sendMessage RestoreNextMinimizedWin),
        -- App launcher
        ((modm,                 xK_p),                    shellPrompt menuXPConfig),
        ((modm .|. controlMask, xK_p),                    shellPrompt menuXPConfig { defaultText = "urxvt -e " }),
        ((modm .|. shiftMask,   xK_p),                    spawn "xfce4-appfinder"),
        -- App shortcut
        ((modm .|. controlMask, xK_l),                    spawn "xlock"),
        ((modm .|. controlMask, xK_w),                    spawn "$BROWSER"),
        ((modm .|. controlMask, xK_e),                    spawn "$VISUAL"),
        ((modm .|. controlMask, xK_f),                    spawn "pcmanfm"),
        ((modm .|. controlMask, xK_m),                    spawn "emacs -T mail -f my/mu4e-start"),
        ((modm .|. controlMask, xK_j),                    spawn "emacs -T jabber -f my/jabber-start"),
        -- Audio volume
        ((0,                    xF86XK_AudioRaiseVolume), spawn "audio-volume.sh up"),
        ((0,                    xF86XK_AudioLowerVolume), spawn "audio-volume.sh down")
    ]

-- Define terminal
myTerminal :: String
myTerminal = "emacsclient -c -e '(multi-term)' || emacs -f 'multi-term'"

-- Startup
myStartupHook :: X ()
myStartupHook = do
  -- fix swing apps
  setWMName "LG3D"
  -- background services
  spawnOnce "/usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1"
  spawnOnce "xfce4-power-manager"
  spawnOnce "xfce4-volumed"
  spawnOnce "pcmanfm --daemon-mode"
  spawnOnce "syncthing -no-browser -logflags=3"
  -- systray
  spawnOnce "stalonetray"
  spawnOnce "pasystray"
  spawnOnce "clipit"
  spawnOnce "blueman-applet"
  spawnOnce "nm-applet"

-- Panel
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ prettyPrinter h

prettyPrinter :: Handle -> PP
prettyPrinter h = xmobarPP
    {
      ppOutput = hPutStrLn h,
      ppTitle  = xmobarColor "cyan" "" . shorten 60
    }

-- Do not auto switch workspace
filterEwmhEvent :: (Event -> X All) -> Event -> X All
filterEwmhEvent f e = do
  let mt = ev_message_type e
  a_aw <- getAtom "_NET_ACTIVE_WINDOW"
  if mt == a_aw then do
     return (All True)
  else do
    f e

-- Run XMonad
main :: IO ()
main = do
  xmobarPanel <- spawnPipe "xmobar"
  xmonad $ desktopConfig
       {
         manageHook      = myManageHook <+> manageHook desktopConfig,
         layoutHook      = desktopLayoutModifiers $ myLayout,
         logHook         = myLogHook xmobarPanel <+> logHook desktopConfig,
         modMask         = mod4Mask, -- Rebind mod to windows key
         keys            = myKeys <+> keys desktopConfig,
         terminal        = myTerminal,
         workspaces      = myWorkspaces,
         startupHook     = startupHook desktopConfig >> myStartupHook,
         handleEventHook = filterEwmhEvent $ handleEventHook desktopConfig
       }
