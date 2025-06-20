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
                 :groups {:all {:Title {:fg _G.base0F :style :bold}
                                :FloatBorder {:bg :bg2 :fg :fg3}
                                :NormalFloat {:bg :bg2 :fg :fg1}
                                :Search {:bg :sel1 :fg :bg1}
                                :Cursor {:bg :sel1 :fg :bg1}
                                :CursorLineNC {:bg _G.base00}
                                :TermCursor {:link :Cursor}
                                :WinSeparator {:bg :bg0 :fg :bg0}
                                :rainbowcol1 {:fg _G.base0E}
                                :rainbowcol2 {:fg _G.base0D}
                                :rainbowcol3 {:fg _G.base0C}
                                :rainbowcol4 {:fg _G.base0B}
                                :rainbowcol5 {:fg _G.base0A}
                                :rainbowcol6 {:fg _G.base09}
                                :WhichKeyFloat {:link :FloatBorder}
                                :LspFloatWinNormal {:link :FloatBorder}
                                :TelescopeBorder {:link :FloatBorder}
                                :TelescopeTitle {:fg :sel1}
                                :TelescopeNormal {:bg :bg2}
                                :TelescopeSelectionCaret {:fg :sel1
                                                          :bg _G.base10
                                                          :style :bold}
                                :MatchParen {:fg :sel1
                                             :bg :bg0
                                             :style "bold,underline"}
                                :Folded {:bg :bg2}
                                :NvimTreeCursorLine {:bg :bg2}
                                :NvimTreeCursorColumn {:link :NvimTreeCursorLine}
                                :NvimTreeModified {:fg _G.base08}
                                :NvimTreeModifiedFile {:link :NvimTreeModified}
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
                                :DiagnosticVirtualLinesError {:link :DiagnosticVirtualTextError}
                                :DiagnosticVirtualLinesWarn {:link :DiagnosticVirtualTextWarn}
                                :DiagnosticVirtualLinesHint {:link :DiagnosticVirtualTextHint}
                                :DiagnosticVirtualLinesInfo {:link :DiagnosticVirtualTextInfo}
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
                                :TroubleNormal {:bg :bg0}
                                :TroubleNormalNC {:link :TroubleNormal}
                                :TroubleText {:link :TroubleNormal}
                                :NotifierTitle {:bg :bg3
                                                :fg :sel1
                                                :style :bold}
                                :NotifierIcon {:link :NotifierTitle}
                                :NotifierContent {:bg :bg3}
                                :TabLineFill {:bg _G.base01 :fg _G.base03}
                                :TabLineSel {:bg _G.base01 :fg _G.base05}
                                :BufferCurrent {:bg _G.base0F
                                                :fg _G.base00
                                                :style :bold}
                                :BufferCurrentIndex {:link :BufferCurrent}
                                :BufferCurrentSign {:link :BufferCurrent}
                                :BufferCurrentMod {:link :BufferCurrent}
                                :BufferVisible {:bg _G.base02 :fg _G.base05}
                                :BufferVisibleIndex {:link :BufferVisible}
                                :BufferVisibleSign {:link :BufferVisible}
                                :BufferVisibleMod {:link :BufferVisible}
                                :BufferInactive {:bg _G.base02 :fg _G.base03}
                                :BufferInactiveIndex {:link :BufferInactive}
                                :BufferInactiveSign {:link :BufferInactive}
                                :BufferInactiveMod {:link :BufferInactive}
                                :NoiceLspProgressTitle {:bg _G.base02
                                                        :fg _G.base03}
                                :IndentBlanklineContextChar {:fg _G.base0F}
                                :CybuFocus {:bg _G.base0F :fg _G.base00}
                                :CybuAdjacent {:bg _G.base02 :fg _G.base03}
                                :CybuBorder {:bg _G.base02}
                                :CybuBackground {:bg _G.base02}
                                :NavicSeparator {:fg _G.base03}
                                :DapStoppedLine {:link :Visual}
                                :NeogitSectionHeader {:fg _G.base0F
                                                      :style :bold}
                                :NeogitChangeModified {:link :DiffChanged}
                                :NeogitChangeAdded {:link :DiffAdded}
                                :NeogitChangeDeleted {:link :DiffRemoved}
                                :NeogitChangeRenamed {:link :DiffChanged}
                                :NeogitChangeUpdated {:link :DiffChanged}
                                :NeogitChangeCopied {:link :DiffFile}
                                :NeogitChangeBothModified {:link :DiffChanged}
                                :NeogitChangeNewFile {:link :DiffNewFile}
                                :RenderMarkdownH1 {:link :rainbowcol1
                                                   :style :bold}
                                :RenderMarkdownH2 {:link :rainbowcol2
                                                   :style :bold}
                                :RenderMarkdownH3 {:link :rainbowcol3
                                                   :style :bold}
                                :RenderMarkdownH4 {:link :rainbowcol4
                                                   :style :bold}
                                :RenderMarkdownH5 {:link :rainbowcol5
                                                   :style :bold}
                                :RenderMarkdownH6 {:link :rainbowcol6
                                                   :style :bold}
                                :RenderMarkdownH1Bg {:bg _G.base01}
                                :RenderMarkdownH2Bg {:bg _G.base01}
                                :RenderMarkdownH3Bg {:bg _G.base01}
                                :RenderMarkdownH4Bg {:bg _G.base01}
                                :RenderMarkdownH5Bg {:bg _G.base01}
                                :RenderMarkdownH6Bg {:bg _G.base01}
                                :EdgyNormal {:link :NormalNC}
                                :NeoTreeModified {:fg _G.base08}
                                :NeoTreeCursorLine {:bg :bg1}
                                :DapUINormalNC {:link :NormalNC}
                                :SnacksDashboardDir {:fg _G.base03}
                                :SnacksDashboardKey {:fg _G.base0F}
                                :SnacksDashboardFile {:fg _G.base0F
                                                      :style :bold}
                                :SnacksDashboardSpecial {:fg _G.base0F}
                                :SnacksDashboardIcon {:fg _G.base0F}
                                :SnacksDashboardFooter {:fg _G.base03}
                                :SnacksDashboardDesc {:fg _G.base0F
                                                      :style :bold}
                                :SnacksNotifierError {:link :NormalFloat}
                                :SnacksNotifierWarn {:link :NormalFloat}
                                :SnacksNotifierInfo {:link :NormalFloat}
                                :SnacksNotifierDebug {:link :NormalFloat}
                                :SnacksNotifierTrace {:link :NormalFloat}
                                :SnacksNotifierBorderError {:link :NormalFloat}
                                :SnacksNotifierBorderWarn {:link :NormalFloat}
                                :SnacksNotifierBorderInfo {:link :NormalFloat}
                                :SnacksNotifierBorderDebug {:link :NormalFloat}
                                :SnacksNotifierBorderTrace {:link :NormalFloat}
                                :SnacksNotifierTitleError {:fg _G.base08
                                                           :bg _G.base02
                                                           :style :bold}
                                :SnacksNotifierTitleWarn {:fg _G.base0A
                                                          :bg _G.base02
                                                          :style :bold}
                                :SnacksNotifierTitleInfo {:fg _G.base0D
                                                          :bg _G.base02
                                                          :style :bold}
                                :SnacksNotifierTitleDebug {:fg _G.base03
                                                           :bg _G.base02
                                                           :style :bold}
                                :SnacksNotifierTitleTrace {:fg _G.base03
                                                           :bg _G.base02
                                                           :style :bold}
                                :SnacksNotifierFooterError {:fg _G.base08
                                                            :bg _G.base02}
                                :SnacksNotifierFooterWarn {:fg _G.base0A
                                                           :bg _G.base02}
                                :SnacksNotifierFooterInfo {:fg _G.base0D
                                                           :bg _G.base02}
                                :SnacksNotifierFooterDebug {:fg _G.base03
                                                            :bg _G.base02}
                                :SnacksNotifierFooterTrace {:fg _G.base03
                                                            :bg _G.base02}
                                :SnacksNotifierIconError {:link :SnacksNotifierFooterError}
                                :SnacksNotifierIconWarn {:link :SnacksNotifierFooterWarn}
                                :SnacksNotifierIconInfo {:link :SnacksNotifierFooterInfo}
                                :SnacksNotifierIconDebug {:link :SnacksNotifierFooterDebug}
                                :SnacksNotifierIconTrace {:link :SnacksNotifierFooterTrace}
                                :SnacksNotifierHistory {:link :NormalFloat}
                                :SnacksNotifierHistoryTitle {:bg _G.base02
                                                             :link :Title}
                                :SnacksInputTitle {:link :Title}
                                :SnacksInputIcon {:fg _G.base0F}
                                :SnacksPickerCursorLine {:bg _G.base10}
                                :SnacksPickerListCursorLine {:link :SnacksPickerCursorLine}
                                :SnacksPickerPreviewCursorLine {:link :SnacksPickerCursorLine}
                                :SnacksPickerBorder {:fg _G.base01
                                                     :bg _G.base01}
                                :SnacksPickerTitle {:link :Title}
                                :SnacksPickerPrompt {:fg _G.base0F}
                                :SnacksPickerMatch {:fg _G.base0F}
                                :SnacksPickerDir {:fg _G.base03}
                                :SnacksPickerTotals {:fg _G.base03}
                                :SnacksPickerKeymapRhs {:fg _G.base03}
                                :SnacksPickerPathHidden {:fg _G.base03}
                                :SnacksPickerPathIgnored {:fg _G.base03}
                                :SnacksPickerUnselected {:fg _G.base03}
                                :SnacksPickerGitStatusIgnored {:fg _G.base03}
                                :SnacksPickerGitStatusUntracked {:fg _G.base03}
                                :SnacksPickerDimmed {:fg _G.base03}
                                :GlancePreviewNormal {:link :NormalFloat}
                                :GlancePreviewCursorLine {:bg _G.base00}
                                :GlanceListNormal {:link :NormalFloat}
                                :GlanceBorderTop {:bg _G.base00}
                                :GlanceIndent {:link :NonText}
                                :LspReferenceText {:bg _G.base01}
                                :LspReferenceRead {:link :LspReferenceText}
                                :LspReferenceWrite {:link :LspReferenceText}
                                :OutlineGuides {:link :NonText}
                                :OutlineCurrent {:fg _G.base0F :style :bold}
                                :OutlineFoldMarker {:fg _G.base03}}}})

(nightfox.compile)
(vim.cmd.colorscheme :nightfox)
