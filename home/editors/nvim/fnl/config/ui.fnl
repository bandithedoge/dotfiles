[(_G.use :sindrets/diffview.nvim {:cmd [:DiffviewOpen]})
 ;;
 (_G.use :folke/edgy.nvim
         {:event :VeryLazy
          :opts {:wo {:winbar false}
                 :animate {:enabled false}
                 :bottom [:Trouble :dap-repl :dapui_console]
                 :left [:neo-tree]
                 :right [:dapui_scopes
                         :dapui_breakpoints
                         :dapui_stacks
                         :dapui_watches]}})
 ;;
 (_G.use :numToStr/FTerm.nvim
         {:keys [(_G.key :<leader>T
                         #(let [fterm (require :FTerm)] (fterm.toggle))
                         {:desc :Terminal})]
          :opts {:hl :NormalFloat :border :solid}})
 ;;
 (_G.use :lewis6991/gitsigns.nvim
         {:event :LazyFile
          :opts {:signs {:add {:text "▎"}
                         :change {:text "▎"}
                         :delete {:text ""}
                         :topdelete {:text ""}
                         :changedelete {:text "▎"}
                         :untracked {:text "▎"}}
                 :signs_staged {:add {:text "▎"}
                                :change {:text "▎"}
                                :delete {:text ""}
                                :topdelete {:text ""}
                                :changedelete {:text "▎"}}
                 :diff_opts {:internal true}}})
 ;;
 (_G.use :lukas-reineke/indent-blankline.nvim
         {:dependencies [(_G.use :nvim-treesitter/nvim-treesitter)
                         (_G.use :HiPhish/rainbow-delimiters.nvim)]
          :event :LazyFile
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
                                       :trouble
                                       :notify]
                           :buftypes [:terminal :prompt]}
                 :indent {:char "│"}}
          :config #(let [ibl (require :ibl)
                         hooks (require :ibl.hooks)]
                     (ibl.setup $2)
                     (hooks.register hooks.type.SCOPE_HIGHLIGHT
                                     hooks.builtin.scope_highlight_from_extmark))})
 ;;
 (_G.use :NeogitOrg/neogit
         {:dependencies [(_G.use :nvim-lua/plenary.nvim)
                         (_G.use :nvim-telescope/telescope.nvim)]
          :cmd :Neogit
          :keys [(_G.key :<leader>g :<cmd>Neogit<cr> {:desc :Git})]
          :opts {:graph_style :unicode
                 :signs {:hunk ["" ""]
                         :item ["󰅂" "󰅀"]
                         :section ["󰅂" "󰅀"]}
                 :integrations {:diffview true}}})
 ;;
 (_G.use :brenoprata10/nvim-highlight-colors
         {:event :LazyFile :opts {:enable_tailwind true}})
 ;;
 (_G.use :kevinhwang91/nvim-hlslens {:event :CmdlineEnter :config true})
 ;;
 (_G.use :rcarriga/nvim-notify
         {:event :VeryLazy
          :keys [(_G.key :<leader>fn "<cmd>Telescope notify<cr>"
                         {:desc :Notifications})]
          :opts {:stages :slide
                 :fps 60
                 :render :wrapped-compact
                 :timeout 5000
                 :max_height #(math.floor (* vim.o.lines 0.75))
                 :max_width #(math.floor (* vim.o.columns 0.75))}
          :config #(let [notify (require :notify)]
                     (notify.setup $2)
                     (set vim.notify notify))})
 ;;
 (_G.use :rachartier/tiny-devicons-auto-colors.nvim
         {:dependencies [(_G.use :nvim-tree/nvim-web-devicons)]
          :event :VeryLazy
          :opts {:colors [_G.base08
                          _G.base09
                          _G.base0A
                          _G.base0B
                          _G.base0C
                          _G.base0D
                          _G.base0E]}})
 ;;
 (_G.use :folke/todo-comments.nvim
         {:dependencies [(_G.use :nvim-lua/plenary.nvim)
                         (_G.use :folke/trouble.nvim)]
          :cmd [:TodoTrouble :TodoTelescope]
          :event :LazyFile
          :opts {:signs false}})
 ;;
 (_G.use :folke/trouble.nvim
         {:cmd [:TroubleToggle :Trouble]
          :keys [(_G.key :<localleader>t "<cmd>Trouble diagnostics toggle<cr>"
                         {:desc :Diagnostics})]
          :opts {:auto_preview false :use_diagnostic_signs true :padding false}})]

