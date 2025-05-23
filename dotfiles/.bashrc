[[ $- != *i* ]] && return
export PATH=${PATH}:~/puter/path
export PATH=${PATH}:~/.cargo/bin
export LSP_USE_PLISTS=true
export EDITOR="emacsclient -c -a emacs"
export VISUAL="emacsclient -c -a emacs"
export TERMINAL="alacritty"
export BROWSER="firefox"
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export LESSHISTFILE=".history"

export GERBIL_INSTALL_PREFIX=/opt/gerbil
export PATH=${PATH}:${GERBIL_INSTALL_PREFIX}/bin

export XCURSOR_THEME="miku-cursor-linux"
export XCURSOR_PATH=${XCURSOR_PATH}:~/.local/share/icons

export MPD_HOST="localhost"
export MPD_PORT="6669"

alias cls="tput reset"
alias ls='ls --color=auto'
alias ll="ls -lah"
alias grep='grep --color=auto'
alias tmux="tmux -2"

PS1='[\u@\h \W]\$ '
