fundle plugin 'PatrickF1/fzf.fish'
fundle plugin 'jorgebucaran/autopair.fish'
fundle init

cbonsai -p

set fish_greeting
set TERM "xterm-256color"

fish_vi_key_bindings

set fish_color_normal           '#d9dceb'
set fish_color_autosuggestion   '#737998'
set fish_color_command          '#69d26e'
set fish_color_error            '#cf4e54'
set fish_color_param            '#d9dceb'

alias s='sudo'
alias c='clear'
alias l='exa -la'
alias ls='exa -la'
alias e='exit'
alias b='bash -c'
alias v='nvim'

alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

alias yi='yay -S'
alias yr='yay -Rns'
alias ys='yay -Ss'
alias yq='yay -Qs'

starship init fish | source
