(local wezterm (require :wezterm))
(local config (wezterm.config_builder))

(set config.colors {:background _G.base00
                    :foreground _G.base05
                    :cursor_bg _G.base0F
                    :cursor_fg _G.base00
                    :cursor_border _G.base0F
                    :selection_bg _G.base05
                    :selection_fg _G.base00
                    :scrollbar_thumb _G.base0F
                    :split _G.base0F
                    :ansi [_G.base01
                           _G.base08
                           _G.base0B
                           _G.base09
                           _G.base0D
                           _G.base0E
                           _G.base0C
                           _G.base06]
                    :brights [_G.base02
                              _G.base12
                              _G.base14
                              _G.base13
                              _G.base16
                              _G.base17
                              _G.base15
                              _G.base0F]})

(set config.font (wezterm.font _G.monoFont))

config

