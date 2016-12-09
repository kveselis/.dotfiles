#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export HISTSIZE=
export ALTERNATE_EDITOR=""
export EDITOR=nvim

export GOPATH=~/go
export PATH="$PATH:$GOPATH/bin"

# gey, blank window fix for JAVA application
export _JAVA_AWT_WM_NONREPARENTING=1
# better font rendering for JAVA apps
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
# export FT2_SUBPIXEL_HINTING=0  # Classic mode
export FT2_SUBPIXEL_HINTING=1  # Infinality mode
#export FT2_SUBPIXEL_HINTING=2  # Default mode

#. /usr/lib/python3.5/site-packages/powerline/bindings/bash/powerline.sh
alias dotfiles='/usr/bin/git --git-dir=/home/artis/.dotfiles/ --work-tree=/home/artis'

eval "$(stack --bash-completion-script stack)"
