[[ $- != *i* ]] && return
export LSP_USE_PLISTS=true
export EDITOR="emacs"
export VISUAL="emacs"
export TERMINAL="alacritty"
export BROWSER="firefox"
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export LESSHISTFILE=".history"
export SBCL_HOME="/usr/lib/sbcl/"

export XCURSOR_THEME="miku-cursor-linux"
export XCURSOR_PATH=${XCURSOR_PATH}:~/.local/share/icons

alias cls="tput reset"
alias ls='ls --color=auto'
alias ll="ls -lah"
alias grep='grep --color=auto'
alias tmux="tmux -2"
PS1='[\u@\h \W]\$ '
