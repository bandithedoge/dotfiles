fish_vi_key_bindings
set fish_greeting

set TERM "xterm-256color"
set EDITOR "nvim"
set PATH $PATH ~/.local/bin ~/.yarn/bin ~/.nimble/bin
set QT_QPA_PLATFORMTHEME "gtk2"

set fish_color_normal           '#d9dceb'
set fish_color_command          '#69d26e'
set fish_color_quote            '#d7953f'
set fish_color_redirection      '#b96be1'
set fish_color_end              '#e1c85c'
set fish_color_error            '#eb585f'
set fish_color_param            '#d9dceb'
set fish_color_comment          '#474dab'

set fish_color_autosuggestion   '#474dab'

set fish_cursor_default     block
set fish_cursor_insert      line
set fish_cursor_replace_one underscore
set fish_cursor_visual      block

bind f '_fzf_search_directory'
bind u '_fzf_search_history'
bind V '_fzf_search_variables'

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

alias pi='paru -S'
alias pr='paru -Rns'
alias ps='paru -Ss'
alias pq='paru -Qs'

starship init fish | source
