HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history
alias cls="tput reset"
alias ls="ls --color=auto"
alias ll="ls -lah"
alias grep="grep --colour=auto"
autoload -U compinit
zstyle ":completion:*" menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)
bindkey -v
export KEYTIMEOUT=1
bindkey -M menuselect "h" vi-backward-char
bindkey -M menuselect "j" vi-up-line-or-history
bindkey -M menuselect "k" vi-down-line-or-history
bindkey -M menuselect "l" vi-forward-char
bindkey -v "^?" backward-delete-char
function zle-keymap-select {
	if [[ ${KEYMAP} == vicmd ]] || [[ $1 = "block" ]]; then
		echo -ne "\e[1 q"
	elif [[ ${KEYMAP} == main ]] || [[ ${KEYMAP} == viins ]] || [[ ${KEYMAP} = "" ]] || [[ $1 = "beam" ]]; then
		echo -ne "\e[5 q"
	fi
}
zle -N zle-keymap-select
zle-line-init() {
	zle -K viins
	echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne "\e[5 q"
preexec(){
	echo -ne "\e[5 q";
}
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^e" edit-command-line
PROMPT="[%n@%m:%~]"
RPROMPT="[%*][%?]"
