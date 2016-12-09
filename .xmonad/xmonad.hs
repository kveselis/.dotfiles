import qualified XMonad.StackSet as W
import qualified Data.Map as M

import Graphics.X11.ExtraTypes.XF86
import Control.Monad
import XMonad.Util.NamedWindows

import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Fullscreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Named

import XMonad.Hooks.DynamicLog             -- for xmobar
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops           -- for quit-monad.sh and notify-send, wmctrl

import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Util.SpawnOnce

import XMonad.Util.Run
import XMonad.Util.EZConfig                -- removeKeys, additionalKeys
import XMonad.Util.Cursor

import Data.List
import System.IO                           -- for xmobar
import System.Exit


myModMask            = mod4Mask
myTerminal           = "urxvt"
myNormalBorderColor  = solarizedBase01
myFocusedBorderColor = solarizedBase1
myBorderWidth        = 0
myIconDir            = "/home/artis/.xmonad/icons/"

myTitleLength = 90

myWorkspaces = map show [1..9]
myActiveWsp = ["➊","➋","➌","➍","➎","➏","➐","➑","➒"]
myInactiveWsp = ["➀","➁","➂","➃","➄","➅","➆","➇","➈"]

-- LibNotify urgency hook
-- Create notification popup when some window becomes urgent.
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "workspace: " ++ idx]



main = do
   xmproc <- spawnPipe "xmobar"
   xmonad $ withUrgencyHook LibNotifyUrgencyHook $ ewmh defaultConfig
      { modMask            = myModMask
      , terminal           = myTerminal
      , borderWidth        = myBorderWidth
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , workspaces         = myWorkspaces
      , startupHook        = myStartupHook
      , manageHook         = manageSpawn <+> manageDocks <+> myManageHook <+> manageHook defaultConfig
      , layoutHook         = smartBorders $ myLayoutHook
      , handleEventHook    = mconcat [docksEventHook, handleEventHook defaultConfig]
      , logHook            = dynamicLogWithPP xmobarPP
           { ppOutput          = hPutStrLn xmproc
           , ppTitle           = xmobarColor solarizedYellow "" . shorten myTitleLength
           , ppCurrent         = xmobarColor solarizedBlue solarizedBase03 . \s -> myActiveWsp!!((read s::Int)-1)
           , ppVisible         = xmobarColor solarizedBase1 solarizedBase03 . \s -> myActiveWsp!!((read s::Int)-1)
           , ppHidden          = xmobarColor solarizedBase01 solarizedBase03 . \s -> myActiveWsp!!((read s::Int)-1)
           , ppHiddenNoWindows = xmobarColor solarizedBase02 solarizedBase03  . \s -> myInactiveWsp!!((read s::Int)-1)
           , ppSep             = xmobarColor solarizedBase01 "" " "
	   , ppUrgent          = xmobarColor solarizedRed solarizedBase03 . \s -> myActiveWsp!!((read s::Int)-1)
           , ppLayout          =
	              (\x -> case x of
                         "Full"  -> "<icon=" ++ myIconDir ++ "layout-full.xbm/>"
                         "Mirror SmartSpacing 10 Tall" -> "<icon=" ++ myIconDir ++ "layout-mirror-bottom.xbm/>"
                         "Mirror ResizableTall" -> "<icon=" ++ myIconDir ++ "layout-mirror-top.xbm/>"
                         "SmartSpacing 10 Tall" -> "<icon=" ++ myIconDir ++ "layout-tall-right.xbm/>"
                         "ResizableTall"        -> "<icon=" ++ myIconDir ++ "layout-tall-left.xbm/>"
                         "Simple Float"         -> "~"
                         _                      -> pad x
                      )
           }
      }
      `additionalKeysP`
      [ ("M-p",             spawn "dmenu_recent -fn PragmataPro-13")
      , ("M-S-p",           spawn "passmenu -fn PragmataPro-13")
      , ("M-r",             spawn "rofi -font 'Pragmata Pro 12' -combi-modi window,drun,run -show combi")
      , ("M-b",             sendMessage ToggleStruts)
      , ("M-c",             spawn "urxvtc -name weechat -e weechat")
      , ("M-o",             spawn "urxvtc -name ncmpcpp -e ncmpcpp")
      , ("M-S-l",           spawn "slock") -- Lock the screen
      , ("M-S-q",           spawn "~/.xmonad/scripts/quit-xmonad.sh") -- Quit xmonad nicely
      , ("M-S-<Backspace>", spawn "/bin/systemctl suspend")
      , ("M-S-<Delete>",    spawn "/bin/systemctl hibernate")
      , ("M-S-e",           spawn "emacs")
      , ("M-S-g",           spawn "chromium")
      , ("M-S-f",           spawn "firefox")
      , ("M-S-A-q",         io (exitWith ExitSuccess))
      , ("M-q",             spawn $ unlines [
	    "xmonad --recompile"
           , "if [ $? -eq 0 ]; then"
           , "    xmonad --restart"
           , "    NID=$(notify-send -u low 'XMonad' 'Recompiled and restarted.')"
           , "else"
           , "    notify-send -u critical \"XMonad recompilation failed!\" \"$(cat ~/.xmonad/xmonad.errors)\""
           , "fi"
           ]
        )
       -- Brightness Keys
      , ("<XF86Display>",           spawn "arandr")
      , ("<XF86MonBrightnessUp>"  , spawn "xbacklight + 5 -time 100 -steps 1")
      , ("<XF86MonBrightnessDown>", spawn "xbacklight - 5 -time 100 -steps 1")
      , ("<XF86AudioLowerVolume>",  spawn "~/.config/xmobar/scripts/vol-control down")
      , ("<XF86AudioRaiseVolume>",  spawn "~/.config/xmobar/scripts/vol-control up")
      , ("<XF86AudioMute>",         spawn "~/.config/xmobar/scripts/vol-control toggle")

       -- CycleWS setup, keybindings for
      , ("M-C-<R>", nextWS)
      , ("M-C-<L>", prevWS)
      , ("M-S-<R>", shiftToNext >> nextWS)
      , ("M-S-<L>", shiftToPrev >> prevWS)
      , ("M-S-.",   shiftNextScreen)
      , ("M-S-,",   shiftPrevScreen)
      , ("M-<R>",   nextScreen)
      , ("M-<L>",   prevScreen)
      , ("M-x",     swapNextScreen)
      , ("M-z",     toggleWS)
      ]


myStartupHook = do
  spawnOnce "xrandr --output eDP1 --auto --output DP2 --primary --right-of eDP1 --auto"
  spawn "feh --bg-fill ~/Pictures/wallpapers/default.jpg"
  spawn "pgrep redshift || redshift"
--  spawnOnce "pgrep dunst || dunst" -- probably not needed
  setDefaultCursor xC_left_ptr
  spawn "~/.config/xmobar/scripts/vol-control status"

myManageHook = composeAll
   [ isDialog --> doFloat
   , className =? "VirtualBox" --> doFloat
   , className =? "Qalculate-gtk" --> doFloat
   , className =? "Vlc" --> doFloat
   , className =? "chromium" --> doShift "2"
   , className =? "urxvt" --> doShift "3"
   , appName  =? "weechat" --> doShift "4"
   , appName  =? "XXkb" --> doIgnore
   ]

myLayoutHook = avoidStruts
   $ onWorkspace "2" (Full ||| tiled ||| Mirror tiled)
   $ layouts
   where layouts = tiled ||| Mirror tiled ||| Full
         tiled   = smartSpacing 10 $ gaps [(L,10),(R,10),(U,10),(D,10)] $ Tall nmaster delta ratio
         nmaster = 1
         delta   = 3/100
         ratio   = 3/5


solarizedBase03     = "#002b36"
solarizedBase02     = "#073642"
solarizedBase01     = "#586e75"
solarizedBase00     = "#657b83"
solarizedBase0      = "#839496"
solarizedBase1      = "#93a1a1"
solarizedBase2      = "#eee8d5"
solarizedBase3      = "#fdf6e3"
solarizedYellow     = "#b58900"
solarizedOrange     = "#cb4b16"
solarizedRed        = "#dc322f"
solarizedMagenta    = "#d33682"
solarizedViolet     = "#6c71c4"
solarizedBlue       = "#268bd2"
solarizedCyan       = "#2aa198"
solarizedGreen      = "#859900"
