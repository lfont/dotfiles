-- http://www.nepherte.be/step-by-step-configuration-of-xmonad/

-- Import statements
import XMonad
import XMonad.Config.Desktop
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Minimize
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.Shell

import Control.Monad (liftM2)

import Graphics.X11.ExtraTypes.XF86

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- Define the names of all workspaces
myWorkspaces :: [String]
myWorkspaces = [ "1:www", "2:mail", "3:media", "4", "5" ]

-- Layout
myLayout = Full ||| tiledH ||| tiledV
    where
      tiledH = spacing 2 $ minimize (Tall 1 (3/100) (1/2))
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
menuXPConfig :: XPConfig
menuXPConfig = greenXPConfig
    {
      font              = "xft:Terminus-12:Regular",
      height            = 20,
      position          = Top,
      promptBorderWidth = 0,
      promptKeymap      = emacsLikeXPKeymap
    }

myKeys (XConfig {modMask = modm}) = M.fromList $
    [
        -- Reload configuration
        ((modm,                 xK_q),                    spawn "xmonad --recompile && xmonad --restart"),
        -- Minimize window
        ((modm,                 xK_d),                    withFocused minimizeWindow),
        ((modm .|. shiftMask,   xK_d),                    sendMessage RestoreNextMinimizedWin),
        -- Show urgency
        ((modm,                 xK_BackSpace),            focusUrgent),
        ((modm .|. shiftMask,   xK_BackSpace),            clearUrgents),
        -- App launcher
        ((modm,                 xK_p),                    shellPrompt menuXPConfig),
        ((modm .|. controlMask, xK_p),                    prompt ("urxvt" ++ " -e") menuXPConfig),
        ((modm .|. shiftMask,   xK_p),                    spawn "xfce4-appfinder"),
        -- App shortcut
        ((modm .|. controlMask, xK_l),                    spawn "slock"),
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
  setWMName "LG3D"
  spawn     "xmonad-start-once.sh"

-- Panel
myLogHook h = dynamicLogWithPP $ prettyPrinter h

prettyPrinter h = xmobarPP
    {
      ppOutput = hPutStrLn h,
      ppTitle  = xmobarColor "cyan" "" . shorten 55,
      ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
    }

-- Run XMonad
main :: IO ()
main = do
  xmobarPanel <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook $ desktopConfig
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
