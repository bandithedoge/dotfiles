set fish_greeting
set TERM "xterm-256color"

fish_vi_key_bindings

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

bind f '__fzf_search_current_dir'
bind u '__fzf_search_history'
bind V '__fzf_search_shell_variables'

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

alias bi='brew install'
alias bic='brew install --cask'
alias br='brew uninstall'
alias bs='brew search'
alias bq='brew list'

starship init fish | source
