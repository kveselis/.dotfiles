import qualified XMonad.StackSet as W
import qualified Data.Map as M

--import Graphics.X11.ExtraTypes.XF86
--import Control.Monad
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
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops           -- for quit-monad.sh and notify-send, wmctrl

import XMonad.Actions.SpawnOn
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
myIconDir            = "/home/artis/.xmonad/icons"

myTitleLength = 90

myWorkspaces = ["1:dev","2:web","3:term","4:chat","5:","6","7","8","9"]


-- LibNotify urgency hook
-- Create notification popup when some window becomes urgent.
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "Urgent window"]


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
           , ppCurrent         = xmobarColor solarizedBase1 solarizedBase03 . \s -> "●"  -- xmobarColor solarizedBase1 "" . wrap "" ""
           , ppVisible         = xmobarColor solarizedBlue solarizedBase03 . \s -> "◉"
           , ppHidden          = xmobarColor solarizedBase01 solarizedBase03 . \s -> "●"
           , ppHiddenNoWindows = xmobarColor solarizedBase02 solarizedBase03  . \s -> "○"
           , ppSep             = xmobarColor solarizedBase1 "" " | "
	   , ppUrgent          = xmobarColor solarizedRed solarizedBase03 . \s -> "●" --"" . wrap "" ""
           , ppLayout          = const "" .
	              (\x -> case x of
                         "Full"                 -> "^i(" ++ myIconDir ++ "/layout-full.xpm)"
                         "Mirror Tall"          -> "^i(" ++ myIconDir ++ "/layout-mirror-black.xpm)"
                         "Mirror ResizableTall" -> "^i(" ++ myIconDir ++ "/layout-mirror-top.xpm)"
                         "Tall"                 -> "^i(" ++ myIconDir ++ "/layout-tall-black.xpm)"
                         "ResizableTall"        -> "^i(" ++ myIconDir ++ "/layout-tall-left.xpm)"
                         "Simple Float"         -> "~"
                         _                      -> pad x
                      )   
           }
      }
      `additionalKeysP`
      [ ("M-p",     spawn "dmenu_run -fn 'Inconsolata 16'")
      , ("M-b",     sendMessage ToggleStruts)
      , ("M-c",     spawn "urxvtc -name weechat -e weechat")
      , ("M-S-q",   spawn "~/.xmonad/scripts/quit-xmonad.sh" ) -- Quit xmonad nicely
      , ("M-S-A-q", io (exitWith ExitSuccess))
      , ("M-q",     spawn $ unlines [
             "xmonad --recompile"
           , "if [ $? -eq 0 ]; then"
           , "    xmonad --restart"
           , "    notify-send -u low XMonad 'Recompiled and restarted.'"
           , "else"
           , "    notify-send -u critical \"XMonad recompilation failed\" \"\n$(cat ~/.xmonad/xmonad.errors)\""
           , "fi"
           ]
        )
       -- Brightness Keys
      , ("<XF86MonBrightnessUp>"  , spawn "xbacklight + 5 -time 100 -steps 1")
      , ("<XF86MonBrightnessDown>", spawn "xbacklight - 5 -time 100 -steps 1")]


myStartupHook = do
   setDefaultCursor xC_left_ptr
   spawnOnce "feh --bg-scale ~/Pictures/bg1.jpg"
   setWMName "LG3D"
--   spawnOnce "emacs"
--   spawnOnce "chromium"
--   spawnOnce "urxvt"
--   spawnOnce "urxvtc -name weechat -e weechat"
  

myManageHook = composeAll
   [ className =? "chromium" --> doShift "2:web"
   , className =? "urxvt" --> doShift "3:term"
   , resource  =? "weechat" --> doShift "4:chat"
   ]

myLayoutHook = avoidStruts
   $ onWorkspace "1:dev" (Full ||| tiled ||| Mirror tiled)
   $ onWorkspace "2:web" (Full ||| tiled ||| Mirror tiled)
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
