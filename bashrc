#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export PRIMUS_UPLOAD=1
export EKSERIYA_ROOT=$HOME/dev/sw
export SPRITE_ROOT=$EKSERIYA_ROOT
export EKSERIYA_JSON_ROOT=$EKSERIYA_ROOT/siliconwars/client/sw2/res/json
export EKSERIYA_ASSET_ROOT=$EKSERIYA_ROOT/sw2-assets
export SW_TOOLS=$EKSERIYA_ROOT/siliconwars/tools
export BLENDER=/opt/b3d_272/blender
# export BLENDER=blender
export PATH=$PATH:$HOME/dev/sw/siliconwars/tools:/opt/android-ndk-r10
export ALTERNATE_EDITOR=""
export EDITOR=emacsclient
export PVR_TT=/opt/Imagination/PowerVR/GraphicsSDK/PVRTexTool/CLI/Linux_x86_64
export MALI_TT=/opt/etc/bin


function sendkey () {
    if [ $# -eq 1 ]; then
        local key=""
        if [ -f ~/.ssh/id_dsa.pub ]; then
            key=~/.ssh/id_dsa.pub
        elif [ -f ~/.ssh/id_rsa.pub ]; then
            key=~/.ssh/id_rsa.pub
        else
            echo "No public key found" >&2
            return 1
        fi
        ssh $1 'cat >> ~/.ssh/authorized_keys' < $key
    fi
}
