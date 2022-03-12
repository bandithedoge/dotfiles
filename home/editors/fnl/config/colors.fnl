(local nf (require :nightfox))
(local util (require :nightfox.util))

(local colors
       {; background
        :bg base00
        :bg_alt base01
        :bg_popup base02
        :bg_statusline base01
        :bg_sidebar base10
        :bg_float base02
        :bg_visual base03
        :bg_search base0F
        :bg_highlight base02
        :border base02
        ; foreground
        :fg base05
        :fg_alt base04
        :fg_gutter base02
        :fg_sidebar base04
        :border_highlight base0F
        ; standard colors
        :black base01
        :red base08
        :green base0B
        :yellow base0A
        :blue base0D
        :magenta base0E
        :cyan base0C
        :white base05
        :orange base09
        :pink (util.blend base08 base07 0.5)
        ; bright colors
        :black_br base02
        :red_br base12
        :green_br base14
        :yellow_br base13
        :blue_br base16
        :magenta_br base17
        :cyan_br base15
        :white_br base07
        :orange_br (util.blend base12 base13 0.5)
        :pink_br (util.blend base12 base07 0.5)
        ; syntax stuff
        :comment base03
        :variable base05
        :error base08
        :warning base09
        :info base0D
        :hint base0B
        ; git
        :git {:add base0B
              :change base09
              :delete base08
              :conflict base0E
              :ignore base03}
        :gitSigns {:add base0B :change base09 :delete base08}
        :diff {:add base0B :change base09 :delete base08 :text base0D}})

(local hlgroups {; ui
                 :PmenuSel {:fg "${bg}" :bg "${border_highlight}"}
                 :Cursor {:fg "${bg}" :bg "${border_highlight}"}
                 :NormalPopover {:bg "${bg_popup}"}
                 :FloatBorder {:bg "${bg_popup}" :fg "${border_highlight}"}
                 :Folded {:fg "${comment}" :bg "${bg_alt}"}
                 :LineNr {:fg "${comment}"}
                 :CursorLineNr {:fg "${border_highlight}"}
                 :StatusLineNC {:fg "${comment}"}
                 ; telescope
                 :TelescopeSelectionCaret {:fg "${border_highlight}"}
                 :TelescopeSelection {:fg "${harsh}" :bg "${bg}"}
                 :TelescopeNormal {:bg "${bg_popup}"}
                 :TelescopeBorder {:bg "${bg_popup}"}
                 ; indents
                 :IndentBlankline {:fg "${bg_alt}"}
                 :IndentBlanklineChar {:fg "${bg_alt}"}
                 :IndentBlanklineContextChar {:fg "${border_highlight}"}
                 ; rainbow parentheses
                 :rainbowcol1 {:fg "${magenta_br}"}
                 :rainbowcol2 {:fg "${blue_br}"}
                 :rainbowcol3 {:fg "${cyan_br}"}
                 :rainbowcol4 {:fg "${green_br}"}
                 :rainbowcol5 {:fg "${yellow_br}"}
                 :rainbowcol6 {:fg "${orange_br}"}
                 :rainbowcol7 {:fg "${red_br}"}
                 :rainbowcol8 {:fg "${pink}"}})

(nf.setup {:fox :nightfox
           :styles {:comments :italic
                    :strings :italic
                    :functions :italic
                    :keywords :bold}
           :inverse {:match_paren true}
           : colors
           : hlgroups})

(nf._colorscheme_load)
