pfetch

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH="/home/bandithedoge/.oh-my-zsh"
export EDITOR='nvim'
ZSH_THEME="powerlevel10k/powerlevel10k"
ZSH_AUTOSUGGEST_STRATEGY=(history)


plugins=(
	git
	archlinux
	colored-man-pages
)

source $ZSH/oh-my-zsh.sh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

alias ls="exa -la"
alias c="clear"
alias btw="figlet 'BTW, I use' && echo '' && neofetch"
alias ds="~/.emacs.d/bin/doom sync"

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
if [ -e /home/bandithedoge/.nix-profile/etc/profile.d/nix.sh ]; then . /home/bandithedoge/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
