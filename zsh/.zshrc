pfetch

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH="/home/bandithedoge/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k"

plugins=(
	git
	archlinux
	colored-man-pages
)

source $ZSH/oh-my-zsh.sh

source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

ZSH_AUTOSUGGEST_STRATEGY=(history)

export EDITOR='nvim'

alias ls="exa -la"
alias c="clear"
alias upgd="archnews && sudo reflector -c "Poland" -f 12 -l 10 -n 12 --save /etc/pacman.d/mirrorlist && yay -Syu && flatpak update && yay -Sc --noconfirm && sudo pacman -Rs $(pacman -Qtdq) --noconfirm"
alias btw="figlet 'BTW, I use' && echo '' && neofetch"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
