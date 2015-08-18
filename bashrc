#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
alias ls='ls --color=auto'
alias emc='emacsclient -c'
alias emt='emacsclient -t'
PS1='[\u@\h \W]\$ '

export ALTERNATE_EDITOR=""
export EDITOR=emacsclient

export GTAGSLABEL=pygments

# Add environment variable COCOS_X_ROOT for cocos2d-x
export COCOS_X_ROOT=$HOME/dev/cocos/cocos2d-x
export PATH=$COCOS_X_ROOT:$PATH

# Add environment variable COCOS_CONSOLE_ROOT for cocos2d-x
export COCOS_CONSOLE_ROOT=$COCOS_X_ROOT/tools/cocos2d-console/bin
export PATH=$COCOS_CONSOLE_ROOT:$PATH

# Add environment variable COCOS_TEMPLATES_ROOT for cocos2d-x
export COCOS_TEMPLATES_ROOT=$COCOS_X_ROOT/templates
export PATH=$COCOS_TEMPLATES_ROOT:$PATH

# Add environment variable ANT_ROOT for cocos2d-x
export NDK_ROOT=$HOME/dev/cocos/Android/Ndk
export ANDROID_SDK_ROOT=$HOME/dev/cocos/Android/Sdk
export ANT_ROOT=/usr/bin
export PATH=$ANDROID_SDK_ROOT/platform-tools:$ANDROID_SDK_ROOT/tools:$ANT_ROOT:$PATH

# Add environment variable for sdkbox.
export PATH=$HOME/dev/cocos/sdkbox:$PATH
