-- Import statements
import XMonad

import XMonad.Config.Desktop

import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.SimplestFloat

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.FuzzyWindowPrompt

import qualified XMonad.StackSet as W

import qualified Data.Map as M
import Data.Monoid

import Control.Monad (liftM2)

import Graphics.X11.ExtraTypes.XF86

import System.IO

-- Define the names of all workspaces
myWorkspaces :: [String]
myWorkspaces = [ "1", "2", "3:www", "4:chats", "5:float" ]

-- Layout
myLayout = onWorkspace "5:float" (deco simplestFloat) $ Full ||| deco tiledH
    where
      theme  = defaultTheme { activeColor = "#444444"
                            , inactiveColor = "#222222"
                            , activeBorderColor = "#444444"
                            , inactiveBorderColor = "#222222"
                            , activeTextColor = "white"
                            , inactiveTextColor = "#E0E0E0"
                            , fontName = "xft:Hack-8:Normal"
                            , decoWidth = 250
                            , decoHeight = 18 }
      deco   = simpleDeco shrinkText theme
      tiledH = spacing 2 $ Tall 1 (3/100) (1/2)

-- Windows management
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll . concat $
               [
                [ className =? c --> viewShift "3:www"   | c <- myShiftClassesWWW  ],
                [ title     =? t --> viewShift "3:www"   | t <- myShiftTitlesWWW   ],
                [ title     =? t --> viewShift "4:chats" | t <- myShiftTitlesChats ],
                [ resource  =? c --> doFloat             | c <- myFloatResources   ],
                [ resource  =? r --> doIgnore            | r <- myIgnoreResources  ]
               ]
    where
      viewShift          = doF . liftM2 (.) W.greedyView W.shift
      myShiftClassesWWW  = [ "Firefox", "Chromium" ]
      myShiftTitlesWWW   = [ "pass" ]
      myShiftTitlesChats = [ "mail", "jabber" ]
      myFloatResources   = [ "xfce4-appfinder" ]
      myIgnoreResources  = [ "stalonetray" ]

-- Keys binding
menuXPConfig :: XPConfig
menuXPConfig = greenXPConfig
    {
      font         = "xft:Hack-10:Normal",
      height       = 20,
      position     = Top,
      promptKeymap = emacsLikeXPKeymap
    }

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {modMask = modm}) = M.fromList $
    [
        -- Reload configuration
        ((modm,                            xK_q),                    spawn "xmonad --recompile && xmonad --restart"),
        -- App/Window prompt
        ((modm,                            xK_p),                    shellPrompt menuXPConfig),
        ((modm .|. mod1Mask,               xK_p),                    fuzzyWindowPromptGoto menuXPConfig),
        ((modm .|. mod1Mask .|. shiftMask, xK_p),                    fuzzyWindowPromptBring menuXPConfig),
        -- App shortcut
        ((modm .|. controlMask,            xK_l),                    spawn "slock"),
        ((modm .|. controlMask,            xK_w),                    spawn "$BROWSER"),
        ((modm .|. controlMask,            xK_e),                    spawn "$VISUAL"),
        ((modm .|. controlMask,            xK_f),                    spawn "pcmanfm"),
        ((modm .|. controlMask,            xK_m),                    spawn "mail.sh"),
        ((modm .|. controlMask,            xK_j),                    spawn "jabber.sh"),
        ((modm .|. controlMask,            xK_p),                    spawn "pass.sh"),
        -- Audio volume
        ((0,                               xF86XK_AudioRaiseVolume), spawn "audio-volume.sh up"),
        ((0,                               xF86XK_AudioLowerVolume), spawn "audio-volume.sh down")
    ]

-- Define terminal
myTerminal :: String
myTerminal = "urxvt"

-- Startup
myStartupHook :: X ()
myStartupHook = do
  -- fix swing apps
  setWMName "LG3D"
  -- background services
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
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
  spawnOnce "skype"

-- Panel
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ prettyPrinter h

prettyPrinter :: Handle -> PP
prettyPrinter h = xmobarPP
    {
      ppOutput = hPutStrLn h,
      ppTitle  = xmobarColor "cyan" "" . shorten 60,
      ppLayout = \_ -> ""
    }

-- Do not auto switch workspace
filterEwmhEvent :: (Event -> X All) -> Event -> X All
filterEwmhEvent f e = do
  let mt = ev_message_type e
  a_aw <- getAtom "_NET_ACTIVE_WINDOW"
  if mt == a_aw
  then do
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
