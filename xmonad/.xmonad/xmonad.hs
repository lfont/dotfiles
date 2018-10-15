import XMonad

import XMonad.Config.Desktop

import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing

import qualified XMonad.StackSet as W

import qualified Data.Map as M
import Data.Monoid

import Control.Monad (liftM2)

import Graphics.X11.ExtraTypes.XF86

import System.IO

-- Run XMonad
main :: IO ()
main = do
  xmobarPanel <- spawnPipe "xmobar"
  xmonad $
    desktopConfig
      { modMask = mod4Mask -- Rebind mod to windows key
      , keys = myKeys <+> keys desktopConfig
      , terminal = myTerminal
      , workspaces = myWorkspaces
      , manageHook = myManageHook <+> manageHook desktopConfig
      , layoutHook = desktopLayoutModifiers $ myLayout
      -- this must be in this order, docksEventHook must be last
      -- https://unix.stackexchange.com/questions/288037/xmobar-does-not-appear-on-top-of-window-stack-when-xmonad-starts/303242#303242
      , handleEventHook = handleEventHook desktopConfig <+> docksEventHook
      , logHook = myLogHook xmobarPanel <+> logHook desktopConfig
      , startupHook = startupHook desktopConfig >> myStartupHook
      }

-- Define the names of all workspaces
myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5"]

-- Layout
myLayout = onWorkspace "5" (deco simplestFloat) $ tiledH ||| tiledV ||| Full
  where
    tiledV = Mirror tiledH
    tiledH = spacing $ Tall 1 (3 / 100) (2 / 3)
    spacing = spacingRaw False (uniformBorder 0) False (uniformBorder 2) True
    uniformBorder i = Border i i i i
    deco = simpleDeco shrinkText theme
    theme =
      def
        { activeColor = "#444444"
        , inactiveColor = "#222222"
        , activeBorderColor = "#444444"
        , inactiveBorderColor = "#222222"
        , activeTextColor = "white"
        , inactiveTextColor = "#E0E0E0"
        , fontName = "xft:Hack-9:Normal"
        , decoWidth = 300
        , decoHeight = 30
        }

-- Windows management
myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll . concat $
  [ [className =? c --> viewShift "1" | c <- ["Firefox", "Chromium"]]
  , [className =? t --> viewShift "2" | t <- ["Slack"]]
  , [resource =? c --> doFloat | c <- []]
  , [resource =? r --> doIgnore | r <- ["stalonetray"]]
  ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift

-- Keys binding
myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {modMask = modm}) =
  M.fromList $
       -- Lock screen
  [ ((modm .|. shiftMask, xK_l), spawn "slock")
       -- App/Window prompt
  , ((modm, xK_p), spawn "dmenu_run -fn 'Hack-10' -p 'run: ' -l 10")
  , ( (modm .|. shiftMask, xK_p)
    , spawn "dmenu_launcher.sh -fn 'Hack-10' -p 'launch: ' -l 10")
  , ((modm, xK_g), spawn "dmenu_goto_window.sh -fn 'Hack-10' -p 'goto: ' -l 10")
       -- Audio volume
  , ((0, xF86XK_AudioRaiseVolume), spawn "audio-volume.sh up")
  , ((0, xF86XK_AudioLowerVolume), spawn "audio-volume.sh down")
       -- move focus up or down the window stack
  , ((mod1Mask, xK_Tab), windows W.focusDown) -- %! Move focus to the next window
  , ((mod1Mask .|. shiftMask, xK_Tab), windows W.focusUp) -- %! Move focus to the previous window
  ]

-- Define terminal
myTerminal :: String
myTerminal = "st -f 'Hack:size=10:antialias=true:autohint=true' -g 120x34"

-- Startup
myStartupHook :: X ()
myStartupHook
  -- fix swing apps
 = do
  setWMName "LG3D"
  -- background services
  spawnOnce "stalonetray"
  spawnOnce "dex -ae xmonad"

-- Panel
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ prettyPrinter h

prettyPrinter :: Handle -> PP
prettyPrinter h =
  xmobarPP
    { ppOutput = hPutStrLn h
    , ppTitle = xmobarColor "cyan" "" . shorten 60
    , ppHiddenNoWindows = xmobarColor "grey" ""
    }
