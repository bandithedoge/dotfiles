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
                    :visual_bell _G.base0F
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

(set config.animation_fps 60)
(set config.audible_bell :Disabled)
(set config.bold_brightens_ansi_colors :No)
(set config.cell_width 0.9)
(set config.check_for_updates false)
(set config.command_palette_bg_color _G.base02)
(set config.command_palette_fg_color _G.base05)
(set config.default_cursor_style :SteadyBar)
(set config.font (wezterm.font _G.monoFont))
(set config.font_size 12)
(set config.hide_tab_bar_if_only_one_tab true)
(set config.show_tab_index_in_tab_bar false)
(set config.term :wezterm)

(set config.visual_bell {:fade_in_duration_ms 75
                         :fade_in_function :EaseOut
                         :fade_out_duration_ms 75
                         :fade_out_function :EaseOut})

(set config.window_frame {:inactive_titlebar_bg _G.base10
                          :inactive_titlebar_fg _G.base03
                          :active_titlebar_bg _G.base10
                          :active_titlebar_fg _G.base03
                          :button_bg _G.base00
                          :button_fg _G.base03
                          :button_hover_bg _G.base0F
                          :button_hover_fg _G.base00
                          :font (wezterm.font _G.uiFont)
                          :font_size 11})

(set config.window_padding {:left 5 :right 5 :top 5 :bottom 5})

config

