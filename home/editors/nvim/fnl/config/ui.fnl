(require-macros :hibiscus.vim)

[(_G.use :lukas-reineke/indent-blankline.nvim
         {:dependencies [(_G.use :nvim-treesitter/nvim-treesitter)]
          :event [:BufReadPre :BufNewFile]
          :opts {:scope {:show_start false
                         :show_end false
                         :highlight [:rainbowcol1
                                     :rainbowcol2
                                     :rainbowcol3
                                     :rainbowcol4
                                     :rainbowcol5
                                     :rainbowcol6
                                     :rainbowcol7]}
                 :exclude {:filetypes [:help
                                       :TelescopePrompt
                                       :neo-tree
                                       :lazy
                                       :mason
                                       :trouble]
                           :buftypes [:terminal]}
                 :indent {:char "│"}}
          :config #(let [ibl (require :ibl)
                         hooks (require :ibl.hooks)]
                     (ibl.setup $2)
                     (hooks.register hooks.type.SCOPE_HIGHLIGHT
                                     hooks.builtin.scope_highlight_from_extmark))})
 ;;
 ; (_G.use :NvChad/nvim-colorizer.lua
 ;         {:event [:BufReadPre :BufNewFile]
 ;          :opts {:user_default_options {:css true}}})
 ;;
 (_G.use :lewis6991/gitsigns.nvim
         {:event [:BufReadPre :BufNewFile] :opts {:diff_opts {:internal true}}})
 ;;
 (_G.use :numToStr/FTerm.nvim
         {:keys [(_G.key :<leader>T
                         #(let [fterm (require :FTerm)] (fterm.toggle))
                         {:desc :Terminal})]
          :opts {:hl :NormalFloat :border :solid}})
 ;;
 (_G.use :is0n/fm-nvim
         {:keys [(_G.key :<leader>g :<cmd>Lazygit<cr> {:desc :Git})]
          :opts {:ui {:float {:border :solid :float_hl :Normal}}}})
 ;;
 (_G.use :kevinhwang91/nvim-hlslens
         {:event [:BufReadPre :BufNewFile] :config true})
 ;;
 (_G.use :anuvyklack/pretty-fold.nvim
         {:event [:BufReadPre :BufNewFile]
          :opts {:fill_char " "
                 :process_comment_signs false
                 :sections {:left [:content] :right [:number_of_folded_lines]}}})
 ;;
 (_G.use :lewis6991/foldsigns.nvim
         {:event [:BufReadPre :BufNewFile] :config true})
 ;;
 (_G.use :folke/todo-comments.nvim
         {:dependencies [(_G.use :nvim-lua/plenary.nvim)]
          :event [:BufReadPre :BufNewFile]
          :opts {:signs false}})
 ;;
 (_G.use :zbirenbaum/neodim {:event :LspAttach :opts {:blend_color _G.base00}})
 ;;
 (_G.use :folke/trouble.nvim
         {:cmd [:TroubleToggle :Trouble]
          :keys [(_G.key :<localleader>t :<cmd>TroubleToggle<cr>
                         {:desc :Diagnostics})]
          :opts {:auto_preview false :use_diagnostic_signs true}})
 ;;
 (_G.use nil
         {:url "https://git.sr.ht/~whynothugo/lsp_lines.nvim"
          :event [:BufReadPre :BufNewFile]
          :config #(let [lsp-lines (require :lsp_lines)]
                     (lsp-lines.setup)
                     (vim.diagnostic.config {:virtual_text false
                                             :virtual_lines {:highlight_whole_line false
                                                             :only_current_line true}}))}
         :/lsp_lines.nvim)
 (_G.use :akinsho/bufferline.nvim
         {:lazy false
          :keys [(_G.key :<leader><tab> :<cmd>BufferLineCycleNext<cr>
                         {:desc "Next buffer"})
                 (_G.key :<leader><s-tab> :<cmd>BufferLineCyclePrev<cr>
                         {:desc "Previous buffer"})]
          :opts #(let [bufferline (require :bufferline)]
                   {:options {:close_command #(let [bufremove (require :mini.bufremove)]
                                                (bufremove.delete $1))
                              :buffer_close_icon "󰅖"
                              :modified_icon "󰆓"
                              :close_icon "󰅖"
                              :left_trunc_marker "󰅁"
                              :right_trunc_marker "󰅂"
                              :separator_style [" " " "]
                              :style_preset bufferline.style_preset.no_italic
                              :always_show_bufferline false}
                    :highlights {:fill {:bg _G.base10}
                                 :background {:bg _G.base00}
                                 :tab {:bg _G.base00}
                                 :separator {:bg _G.base10}
                                 :separator_visible {:bg _G.base10}
                                 :separator_selected {:bg _G.base10}
                                 :buffer_visible {:fg _G.base04 :bg _G.base02}
                                 :buffer_selected {:fg _G.base0F
                                                   :bg _G.base02
                                                   :bold true}
                                 :modified {:fg _G.base08 :bg _G.base00}
                                 :modified_visible {:fg _G.base08
                                                    :bg _G.base02}
                                 :modified_selected {:fg _G.base08
                                                     :bg _G.base02}
                                 :close_button {:bg _G.base00}
                                 :close_button_visible {:bg _G.base02}
                                 :close_button_selected {:bg _G.base02}
                                 :indicator_visible {:bg _G.base02
                                                     :fg _G.base03}
                                 :indicator_selected {:bg _G.base02
                                                      :fg _G.base0F}
                                 :duplicate {:bg _G.base00 :fg _G.base03}
                                 :duplicate_visible {:bg _G.base02
                                                     :fg _G.base03}
                                 :duplicate_selected {:bg _G.base02
                                                      :fg _G.base0F}}})})]
