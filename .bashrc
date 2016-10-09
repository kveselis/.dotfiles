#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export HISTSIZE=

#. /usr/lib/python3.5/site-packages/powerline/bindings/bash/powerline.sh
alias dotfiles='/usr/bin/git --git-dir=/home/artis/.dotfiles/ --work-tree=/home/artis'

eval "$(stack --bash-completion-script stack)"
