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
export COCOS_X_ROOT=$HOME/bin/cocos/cocos2d-x
export PATH=$COCOS_X_ROOT:$PATH

# Add environment variable COCOS_CONSOLE_ROOT for cocos2d-x
export COCOS_CONSOLE_ROOT=$COCOS_X_ROOT/tools/cocos2d-console/bin
export PATH=$COCOS_CONSOLE_ROOT:$PATH

# Add environment variable COCOS_TEMPLATES_ROOT for cocos2d-x
export COCOS_TEMPLATES_ROOT=$COCOS_X_ROOT/templates
export PATH=$COCOS_TEMPLATES_ROOT:$PATH

# Add environment variable ANT_ROOT for cocos2d-x
export NDK_ROOT=$HOME/bin/android/ndk
export ANDROID_SDK_ROOT=$HOME/bin/android/sdk
export ANT_ROOT=/usr/bin
export PATH=$ANDROID_SDK_ROOT/platform-tools:$ANDROID_SDK_ROOT/tools:$ANT_ROOT:$PATH

# Add environment variable for sdkbox.
export PATH=$HOME/bin/cocos/sdkbox:$PATH

#Z80 Settings
export PATH=$HOME/dev/amstrad/bin/2cdt:$PATH
export PATH=$HOME/dev/amstrad/bin/hex2bin:$PATH
export PATH=$HOME/dev/amstrad/bin/pasmo:$PATH
export PATH=$HOME/dev/amstrad/bin/tape2wav:$PATH
export PATH=$HOME/dev/amstrad/bin/z88dk/bin:$PATH
export ZCCCFG=$HOME/dev/amstrad/bin/z88dk/lib/config
export Z80_OZFILES=$HOME/dev/amstrad/bin/z88dk/lib/config
export SDCC_LIB=/usr/share/sdcc/lib
export SDCC_INCLUDE=/usr/share/sdcc/include

export PATH=/home/kenanb/.gem/ruby/2.4.0/bin:$PATH

export PATH=$HOME/dev/wasm/emsdk:$PATH
export PATH=$HOME/dev/wasm/emsdk/clang/e1.37.21_64bit:$PATH
export PATH=$HOME/dev/wasm/emsdk/node/4.1.1_64bit/bin:$PATH
export PATH=$HOME/dev/wasm/emsdk/emscripten/1.37.21:$PATH

export EMSDK=$HOME/dev/wasm/emsdk
export EM_CONFIG=$HOME/.emscripten
export BINARYEN_ROOT=$HOME/dev/wasm/emsdk/clang/e1.37.21_64bit/binaryen
export EMSCRIPTEN=$HOME/dev/wasm/emsdk/emscripten/1.37.21
