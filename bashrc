#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
[ -n "$XTERM_VERSION" ] && transset-df -a --dec 0.1 >/dev/null
alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# Add environment variable COCOS_CONSOLE_ROOT for cocos2d-x
export COCOS_CONSOLE_ROOT=$HOME/dev/cocos/cocos2d-x/tools/cocos2d-console/bin
export PATH=$COCOS_CONSOLE_ROOT:$PATH

# Add environment variable COCOS_X_ROOT for cocos2d-x
export COCOS_X_ROOT=$HOME/dev/cocos/cocos2d-x
export PATH=$COCOS_X_ROOT:$PATH

# Add environment variable COCOS_TEMPLATES_ROOT for cocos2d-x
export COCOS_TEMPLATES_ROOT=$HOME/dev/cocos/cocos2d-x/templates
export PATH=$COCOS_TEMPLATES_ROOT:$PATH

export GTAGSLABEL=pygments

export PATH=$HOME/.ja2:$PATH
