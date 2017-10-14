#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export HISTSIZE=
export ALTERNATE_EDITOR="nano"
export EDITOR=vim

export GOPATH=~/go
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:/usr/local/texlive/2016/bin/x86_64-linux"
export PATH="$PATH:~/.npm-global/bin"

#if [ -f `which powerline-daemon` ]; then
#  powerline-daemon -q
#  POWERLINE_BASH_CONTINUATION=1
#  POWERLINE_BASH_SELECT=1
#  . /usr/lib/python3.6/site-packages/powerline/bindings/bash/powerline.sh
#fi

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

export HISTCONTROL=ignoreboth:erasedups
shopt -s histappend


# Set config variables first
   GIT_PROMPT_ONLY_IN_REPO=1
   GIT_PROMPT_FETCH_REMOTE_STATUS=0   # uncomment to avoid fetching remote status
   # GIT_PROMPT_SHOW_UPSTREAM=1 # uncomment to show upstream tracking branch
   GIT_PROMPT_SHOW_UNTRACKED_FILES=all # can be no, normal or all; determines counting of untracked files

   # GIT_PROMPT_SHOW_CHANGED_FILES_COUNT=0 # uncomment to avoid printing the number of changed files

   # GIT_PROMPT_STATUS_COMMAND=gitstatus_pre-1.7.10.sh # uncomment to support Git older than 1.7.10

   # GIT_PROMPT_START=...    # uncomment for custom prompt start sequence
   # GIT_PROMPT_END=...      # uncomment for custom prompt end sequence

   # as last entry source the gitprompt script
   # GIT_PROMPT_THEME=Custom # use custom theme specified in file GIT_PROMPT_THEME_FILE (default ~/.git-prompt-colors.sh)
   # GIT_PROMPT_THEME_FILE=~/.git-prompt-colors.sh
   GIT_PROMPT_THEME=Solarized_Art # use theme optimized for solarized color scheme
   source ~/bin/bash-git-prompt/gitprompt.sh

# Only load Liquid Prompt in interactive shells, not from a script or from scp
# Configuration is in ~/.config/liquidpromptrc
#[[ $- = *i* ]] && source ~/bin/liquidprompt/liquidprompt

#source ~/bin/bash-powerline/bash-powerline.sh
