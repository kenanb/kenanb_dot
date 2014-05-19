#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export PRIMUS_UPLOAD=1
export EKSERIYA_ROOT=$HOME/dev/siliconewars
export SPRITE_ROOT=$EKSERIYA_ROOT
export EKSERIYA_JSON_ROOT=$EKSERIYA_ROOT/siliconewars/client/sw2/res/json
export EKSERIYA_ASSET_ROOT=$EKSERIYA__ROOT/asset
export SW_TOOLS=$EKSERIYA_ROOT/siliconewars/tools
export BLENDER=/opt/b270a/blender
# export BLENDER=blender
export PATH=$PATH:$HOME/dev/siliconewars/siliconewars/tools
export ALTERNATE_EDITOR=""
export EDITOR=emacsclient
