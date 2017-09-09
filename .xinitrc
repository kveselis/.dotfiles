#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# Start urxvtd, if installed.
if which urxvtd >/dev/null 2>&1; then
  mkdir -p "$HOME/.urxvt" && rm -f "$HOME/.urxvt/"urxvtd-*
  export RXVT_SOCKET="$HOME/.urxvt/urxvtd-$(hostname)-$$"
  urxvtd -f -o -q
fi

#VBoxClient --clipboard
#VBoxClient --draganddrop
#VboxClient --seamless
#VBoxClient --display
#VBoxClient --checkhostversion

# manages removeable media
udiskie &

# xrandr --output eDP1 --auto --output VGA1 --primary --right-of eDP1 --auto

xset r rate 200 30

# xsetroot -cursor_name left_ptr & #not used if added in xmonad.hs

# hides mouse cursor while typing
xbanish &

# can be done in xmonad.hs
# feh --bg-fill ~/Pictures/wallpapers/default.jpg

# not working on VM
compton -b

#trayer --edge bottom --align right --SetDockType true --expand true --transparent true --alpha 0 --tint 0x000000 --height 12 --width 10  &

start-pulseaudio-x11 &

exec xmonad
