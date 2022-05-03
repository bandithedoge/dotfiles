(module config.colors
        {autoload {nightfox nightfox
                   Shade nightfox.lib.shade
                   color nightfox.lib.color}})

(lambda darken [base]
  (let [col (color.from_hex base)
        darkened (col:shade -0.5)]
    (darkened:to_css)))

(lambda blend [a b]
  (let [base (color.from_hex a)
        blended (base:blend (color.from_hex b) 0.5)]
    (blended:to_css)))

(nightfox.setup {:options {:dim_inactive true
                           :styles {:comments :italic
                                    :keywords :bold
                                    :functions "italic,bold"
                                    :strings :italic}
                           :inverse [:match_paren]}
                 :palettes {:nightfox {:black (Shade.new _G.base01 _G.base02
                                                         _G.base10)
                                       :white (Shade.new _G.base05 _G.base06
                                                         _G.base04)
                                       :red (Shade.new _G.base08 _G.base12
                                                       (darken _G.base08))
                                       :green (Shade.new _G.base0B _G.base14
                                                         (darken _G.base0B))
                                       :yellow (Shade.new _G.base0A _G.base13
                                                          (darken _G.base0A))
                                       :blue (Shade.new _G.base0D _G.base16
                                                        (darken _G.base0D))
                                       :magenta (Shade.new _G.base0E _G.base17
                                                           (darken _G.base0E))
                                       :cyan (Shade.new _G.base0C _G.base15
                                                        (darken _G.base0C))
                                       :orange (Shade.new _G.base09
                                                          (blend _G.base12
                                                                 _G.base13)
                                                          (darken _G.base09))
                                       :pink (Shade.new (blend _G.base08
                                                               _G.base05)
                                                        (blend _G.base12
                                                               _G.base06)
                                                        (darken (blend _G.base08
                                                                       _G.base05)))
                                       :comment _G.base03
                                       :bg0 _G.base10
                                       :bg1 _G.base00
                                       :bg2 _G.base01
                                       :bg3 _G.base02
                                       :bg4 _G.base02
                                       :fg0 _G.base06
                                       :fg1 _G.base05
                                       :fg2 _G.base04
                                       :fg3 _G.base03
                                       :sel0 _G.base02
                                       :sel1 _G.base0F}}
                 :specs {:nightfox {}}
                 :groups {:FloatBorder {:bg :bg2}
                          :TelescopeBorder {:link :FloatBorder}
                          :TelescopeTitle {:fg :sel1}
                          :TelescopeNormal {:bg :bg2}
                          :MatchParen {:fg :sel1
                                       :bg :bg0
                                       :style "bold,underline"}
                          :Folded {:bg :bg2}
                          :NvimTreeIndentMarker {:link :IndentBlanklineChar}
                          :CursorLineNr {:fg :sel1 :style :bold}}})

(nightfox.compile)
(vim.cmd "colorscheme nightfox")
