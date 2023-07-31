(require-macros :hibiscus.vim)

[(_G.use :lukas-reineke/indent-blankline.nvim
         {:dependencies [(_G.use :nvim-treesitter/nvim-treesitter)]
          :event [:BufReadPre :BufNewFile]
          :opts {:show_current_context true
                 :char "│"
                 :use_treesitter true
                 :use_treesitter_scope true
                 :filetype_exclude [:help :TelescopePrompt :neo-tree :lazy :mason :trouble]
                 :buftype_exclude [:terminal]
                 :show_foldtext false
                 :indent_level 30}})
 ;;
 (_G.use :NvChad/nvim-colorizer.lua
         {:event [:BufReadPre :BufNewFile]
          :opts {:filetypes ["*"] :user_default_options {:css true}}})
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
          :opts {:ui {:float {:border :solid :float_hl :NormalPopover}}}})
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
 (_G.use :ghillb/cybu.nvim
         {:keys [(_G.key :<s-tab> :<cmd>CybuLastusedNext<cr>)]
          :opts {:position {:anchor :topcenter}
                 :style {:border :solid}
                 :behavior {:mode {:last_used {:view :paging
                                               :switch :immediate}}}}})
 ;;
 (_G.use :folke/trouble.nvim {:cmd [:TroubleToggle :Trouble]
                              :keys [(_G.key :<localleader>t "<cmd>TroubleToggle<cr>" {:desc :Diagnostics})]
                              :opts {:auto_preview false
                                     :use_diagnostic_signs true}})
 ;;
 (_G.use nil {:url "https://git.sr.ht/~whynothugo/lsp_lines.nvim"
              :config #(let [lsp-lines (require :lsp_lines)]
                        (lsp-lines.setup)
                        (vim.diagnostic.config {:virtual_text false
                                                :virtual_lines {:highlight_whole_line false
                                                                :only_current_line true}}))}
             :/lsp_lines.nvim)]
