module Main (main) where

import XMonad

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Util.ExtensibleState as State

import Data.List
import System.IO                           -- for xmobar
import System.Exit
import Graphics.X11.ExtraTypes.XF86
import Control.Monad

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
import XMonad.Actions.Promote

import XMonad.Util.Dmenu
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.EZConfig                -- removeKeys, additionalKeys
import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad (NamedScratchpad (..),
                                    customFloating,
                                    namedScratchpadFilterOutWorkspacePP, -- Do not display NSP workspace
                                    namedScratchpadAction,
                                    namedScratchpadManageHook)



myModMask            = mod4Mask
myTerminal           = "urxvt"
myTerminalc          = "urxvtc"
myNormalBorderColor  = solarizedBase01
myFocusedBorderColor = solarizedBase1
myBorderWidth        = 0
myIconDir            = "/home/artis/.xmonad/icons/"

myTitleLength = 90

myWorkspaces = map show [1..9]

myActiveWsp = clickable $ ["➊","➋","➌","➍","➎","➏","➐","➑","➒"]
myInactiveWsp = clickable $ ["➀","➁","➂","➃","➄","➅","➆","➇","➈"]
clickable lst =
  ["<action=xdotool key super+" ++ show i ++ ">" ++ w ++ "</action>" |
   (i, w) <- zip [1..(length lst)] lst]


-- namedscratchpad
myScratchpads =
  [
      NS "scratchTerm"    spawnTerm findTerm (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "scratchHtop"    spawnHtop findHtop (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "scratchNcmpcpp" spawnNcmpcpp findNcmpcpp (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "scratchMixer"   spawnMixer findMixer (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "scratchPavuctl" spawnPavuctl findPavuctl (customFloating $ W.RationalRect (1/3) (1/4) (1/3) (1/2))
    , NS "scratchQalc"    spawnQalc findQalc (customFloating $ W.RationalRect (1/3) (1/4) (1/3) (1/2))
  ] where
      spawnTerm    = myTerminal ++ " -background rgba:0000/0000/0200/f000 -name scratchTerm"
      findTerm     = resource =? "scratchTerm"
      spawnHtop    = myTerminalc ++  " -background rgba:0000/0000/0200/f000 -name scratchHtop -e htop"
      findHtop     = resource =? "scratchHtop"
      spawnNcmpcpp = myTerminalc ++ " -name scratchNcmpcpp -e ncmpcpp"
      findNcmpcpp  = resource =? "scratchNcmpcpp"
      spawnMixer   = myTerminalc ++ " -name scratchMixer -e alsamixer"
      findMixer    = resource =? "scratchMixer"
      spawnPavuctl = "pavucontrol"
      findPavuctl  = resource =? "pavucontrol"
      spawnQalc    = "qalculate-gtk"
      findQalc     = resource =? "qalculate-gtk"


-- Create notification popup when some window becomes urgent.
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "workspace: " ++ idx]

main = do
   xmproc <- spawnPipe "xmobar"
   xmonad $ withUrgencyHook LibNotifyUrgencyHook $ ewmh def
      { modMask            = myModMask
      , terminal           = myTerminal
      , borderWidth        = myBorderWidth
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , workspaces         = myWorkspaces
      , startupHook        = myStartupHook
      , manageHook         = manageSpawn <+> manageDocks <+> myManageHook
                             <+> namedScratchpadManageHook myScratchpads <+> manageHook defaultConfig
      , layoutHook         = smartBorders $ myLayoutHook
      , handleEventHook    = mconcat [docksEventHook, handleEventHook def]
      , logHook            = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
           { ppOutput          = hPutStrLn xmproc
           , ppTitle           = xmobarColor solarizedYellow "" . shorten myTitleLength
           , ppCurrent         = xmobarColor solarizedBlue solarizedBase03 . \s -> myActiveWsp!!((read s::Int)-1)
           , ppVisible         = xmobarColor solarizedBase1 solarizedBase03 . \s -> myActiveWsp!!((read s::Int)-1)
           , ppHidden          = xmobarColor solarizedBase1 solarizedBase03 . \s -> myInactiveWsp!!((read s::Int)-1)
           , ppHiddenNoWindows = xmobarColor solarizedBase02 solarizedBase03  . \s -> myInactiveWsp!!((read s::Int)-1)
           , ppSep             = xmobarColor solarizedBase01 "" " "
           , ppUrgent          = xmobarColor solarizedRed solarizedBase03 . \s -> myActiveWsp!!((read s::Int)-1)
           , ppLayout          =
                    (\x -> case x of
                         "Full"  -> "<icon=" ++ myIconDir ++ "layout-full.xbm/>"
                         "Mirror SmartSpacing 5 Tall" -> "<icon=" ++ myIconDir ++ "layout-mirror-bottom.xbm/>"
--                         "Mirror ResizableTall" -> "<icon=" ++ myIconDir ++ "layout-mirror-top.xbm/>"
                         "SmartSpacing 5 Tall" -> "<icon=" ++ myIconDir ++ "layout-tall-right.xbm/>"
--                         "ResizableTall"        -> "<icon=" ++ myIconDir ++ "layout-tall-left.xbm/>"
                         "Simple Float"         -> "~"
                         _                      -> pad x
                     )
           }
      }
      `additionalKeysP`
      [ ("M-p",             spawn "dmenu_recent -fn PragmataPro-13")
      , ("M-S-p",           spawn "passmenu -fn PragmataPro-13")
      , ("M-r",             spawn "rofi -combi-modi window,drun,run -show combi")
      , ("M-S-r",           spawn "autopass")
      , ("M-n",             refresh)
      , ("M-m",             windows W.focusMaster)
      , ("M-S-m",           promote)
      , ("M-b",             sendMessage ToggleStruts)
      , ("M-c",             namedScratchpadAction myScratchpads "scratchQalc")
      , ("M-w",             spawn (myTerminalc ++ " -name weechat -e weechat")) -- -name changes the resource name (so it's not urxvtc)
      , ("M-o",             namedScratchpadAction myScratchpads "scratchNcmpcpp")
      , ("M-<Return>",      namedScratchpadAction myScratchpads "scratchTerm")
      , ("M-i",             namedScratchpadAction myScratchpads "scratchHtop")
      , ("M-<F1>",          namedScratchpadAction myScratchpads "scratchPavuctl")
      , ("M-e",             spawn "emacs")
      , ("M-g",             spawn "google-chrome-unstable")
      , ("M-f",             spawn "firefox")
      , ("M-S-l",           spawn "slock") -- Lock the screen
      , ("M-S-q",           confirm ["-fn", "PragmataPro-13"] "Quit XMonad?" $ spawn "~/.xmonad/scripts/quit-xmonad.sh") -- Try to Quit xmonad nicely
      , ("M-S-<Backspace>", spawn "/bin/systemctl suspend")
      , ("M-S-<Delete>",    spawn "/bin/systemctl hibernate")
      , ("M-q",             confirm ["-fn", "PragmataPro-13"] "Recompile and restart XMonad?" $ spawn $ unlines [
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
      , ("<XF86MonBrightnessUp>"  , spawn "xbacklight +5; notify-send -r 15 'Screen Brightness' \"$(printf '%.*f\n' 0 $(xbacklight -getf))\"")
      , ("<XF86MonBrightnessDown>", spawn "xbacklight -5; notify-send -r 15 'Screen Brightness' \"$(printf '%.*f\n' 0 $(xbacklight -getf))\"")
      , ("<XF86KbdBrightnessUp>",   spawn "xbacklight -inc 25 -ctrl $(xbacklight -list | grep kbd_backlight)")
      , ("<XF86KbdBrightnessDown>", spawn "xbacklight -dec 25 -ctrl $(xbacklight -list | grep kbd_backlight)")
      , ("M-<F10>",                 spawn "xbacklight -inc 10 -ctrl $(xbacklight -list | grep kbd_backlight)")
      , ("M-<F9>",                  spawn "xbacklight -dec 10 -ctrl $(xbacklight -list | grep kbd_backlight)")
      , ("<XF86AudioRaiseVolume>",  spawn "~/.config/xmobar/scripts/vol-control up")
      , ("<XF86AudioLowerVolume>",  spawn "~/.config/xmobar/scripts/vol-control down")
      , ("<XF86AudioMute>",         spawn "~/.config/xmobar/scripts/vol-control toggle")
      , ("M-<F5>", cycleRedShift)
      , ("M-<F6>", cycleRedShift)

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
      , ("M-<Backspace>",     toggleWS)
      ]


myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawn "xrandr --output eDP1 --auto --output DP2 --primary --right-of eDP1 --auto"
  spawn "xset r rate 200 30"
  spawn "killall redshift; redshift -r"
  spawn "~/.config/xmobar/scripts/vol-control status"
  spawn "~/bin/locker"
  spawn "nextwall"

-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
myManageHook = composeAll . concat $
   [ [isDialog --> doCenterFloat]
   , [className =? c --> doCenterFloat | c <- myClassFloats]
   , [appName   =? a --> doCenterFloat | a <- myAppFloats]
   , [resource  =? r --> doIgnore | r <- myIgnores]
   , [className =? "Emacs" --> viewShift (myWorkspaces !! 2)]
   , [className =? "urxvt" --> doShift (myWorkspaces !! 0)]
   , [appName   =? "weechat" --> doShift (myWorkspaces !! 4)]
   , [className   =? "Google-chrome-unstable" --> viewShift (myWorkspaces !! 1)]
   , [appName   =? "wifi-menu" --> doCenterFloat]
   , [isDialog --> doCenterFloat]
   , [isFullscreen --> (doF W.focusDown <+> doFullFloat)]
   ]
   where
     viewShift = doF . liftM2 (.) W.greedyView W.shift
     myClassFloats  = ["Gimp", "VirtualBox", "Vlc", "mpv"]
     myAppFloats = ["wifi-menu"]
     myTitleFloats  = []
     myIgnores = ["desktop_window"]

myLayoutHook = avoidStruts
   $ onWorkspace "2" (Full ||| tiled ||| Mirror tiled)
   $ layouts
   where layouts = tiled ||| Mirror tiled ||| Full
         tiled   = smartSpacing 5 $ gaps [(L,5),(R,5),(U,5),(D,5)] $ Tall nmaster delta ratio
         nmaster = 1
         delta   = 3/100
         ratio   = 3/5


confirm :: [String] -> String -> X () -> X ()
confirm o s f = do
  result <- menuArgs "dmenu" o [s]
  when (result == s) f


nxt :: (Eq a, Enum a, Bounded a) => a -> a
nxt x | x == maxBound = minBound
      | otherwise = succ x

data RedShift = RedShiftEnabled | RedShiftEnabledMax | RedShiftEnabledMin | RedShiftDisabled
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Typeable)

instance ExtensionClass RedShift where
  initialValue = RedShiftEnabled
  extensionType = PersistentExtension

cycleRedShift :: X ()
cycleRedShift = do
  x <- State.get
  let x' = nxt x
  updateRedShift x'
  State.put x'

updateRedShift :: RedShift -> X ()
updateRedShift RedShiftEnabled = do
  spawn "killall redshift; notify-send -r 14 'RedShift' 'ON'; redshift -r"
updateRedShift RedShiftEnabledMax = do
  spawn "killall redshift; notify-send -r 14 'RedShift' 'ON Low'; redshift -r -t 5000:4000"
updateRedShift RedShiftEnabledMin = do
  spawn "killall redshift; notify-send -r 14 'RedShift' 'ON High'; redshift -r -t 4000:3000"
updateRedShift RedShiftDisabled = do
  spawn "pkill -USR1 redshift; notify-send -r 14 'RedShift' 'OFF';"


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

