(local nightfox (require :nightfox))
(local Shade (require :nightfox.lib.shade))
(local color (require :nightfox.lib.color))

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
                 :groups {:all {:FloatBorder {:bg :bg2 :fg :bg2}
                                :NormalFloat {:bg :bg2 :fg :fg1}
                                :Search {:bg :sel1 :fg :bg1}
                                :Cursor {:bg :sel1 :fg :bg1}
                                :VertSplit {:bg _G.base10 :fg _G.base02}
                                :Hlargs {:fg _G.base0F}
                                :WhichKeyFloat {:link :FloatBorder}
                                :LspFloatWinNormal {:link :FloatBorder}
                                :TelescopeBorder {:link :FloatBorder}
                                :TelescopeTitle {:fg :sel1}
                                :TelescopeNormal {:bg :bg2}
                                :TelescopeSelectionCaret {:fg :sel1
                                                          :bg :bg2
                                                          :style :bold}
                                :MatchParen {:fg :sel1
                                             :bg :bg0
                                             :style "bold,underline"}
                                :Folded {:bg :bg2}
                                :NeoTreeCursorLine {:bg :bg2}
                                :NeoTreeModified {:fg _G.base08}
                                :CursorLineNr {:fg :sel1 :style :bold}
                                :CursorLine {:bg :bg0}
                                :PmenuSel {:fg :bg1 :bg :sel1}
                                :DiagnosticUnderlineError {:fg _G.base08
                                                           :style :underline}
                                :DiagnosticUnderlineWarn {:fg _G.base09
                                                          :style :underline}
                                :DiagnosticUnderlineHint {:fg _G.base0B
                                                          :style :underline}
                                :DiagnosticUnderlineInfo {:fg _G.base0D
                                                          :style :underline}
                                :EKaputError {:link :DiagnosticError}
                                :EKaputWarning {:link :DiagnosticWarn}
                                :EKaputHint {:link :DiagnosticHint}
                                :EKaputInfo {:link :DiagnosticInfo}
                                :NotifyBody {:bg :bg3}
                                :NotifyTRACEBody {:link :NotifyBody}
                                :NotifyINFOBody {:link :NotifyBody}
                                :NotifyWARNBody {:link :NotifyBody}
                                :NotifyERRORBody {:link :NotifyBody}
                                :NotifyDEBUGBody {:link :NotifyBody}
                                :NotifyBorder {:bg :bg3 :fg :bg3}
                                :NotifyTRACEBorder {:link :NotifyBorder}
                                :NotifyINFOBorder {:link :NotifyBorder}
                                :NotifyWARNBorder {:link :NotifyBorder}
                                :NotifyERRORBorder {:link :NotifyBorder}
                                :NotifyDEBUGBorder {:link :NotifyBorder}
                                :FidgetTitle {:fg :sel1 :bg :bg3}
                                :FidgetTask {:fg :fg1 :bg :bg3}
                                :TroubleNormal {:bg :bg0}}}})

(nightfox.compile)
(vim.cmd "colorscheme nightfox")
