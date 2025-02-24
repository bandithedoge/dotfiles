(import-macros {: use! : key!} :config.macros)

[(use! :nvim-zh/colorful-winsep.nvim
       {:event :WinLeave
        :opts {:hi {:bg _G.base10 :fg _G.base0F}
               :smooth false
               :symbols ["─" "│" "┌" "┐" "└" "┘"]}})
 ;;
 (use! :folke/edgy.nvim {:event :VeryLazy
                         :opts {:wo {:winbar false}
                                :animate {:enabled false}
                                :bottom [:Trouble :dap-repl :dapui_console]
                                :left [:neo-tree]
                                :right [:dapui_scopes
                                        :dapui_breakpoints
                                        :dapui_stacks
                                        :dapui_watches
                                        :Outline]}})
 ;;
 (use! :lewis6991/gitsigns.nvim
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
 (use! :NeogitOrg/neogit
       {:dependencies [(use! :nvim-lua/plenary.nvim)
                       (use! :sindrets/diffview.nvim)
                       (use! :nvim-telescope/telescope.nvim)]
        :cmd :Neogit
        ; :keys [(key! :<leader>g :<cmd>Neogit<cr> {:desc :Git})]
        :opts {:graph_style :kitty
               :signs {:hunk ["" ""]
                       :item ["󰅂" "󰅀"]
                       :section ["󰅂" "󰅀"]}
               :integrations {:diffview true}}})
 ;;
 (use! :brenoprata10/nvim-highlight-colors
       {:event :LazyFile :opts {:enable_tailwind true}})
 ;;
 (use! :kevinhwang91/nvim-hlslens {:event :CmdlineEnter :config true})
 ;;
 (use! :folke/todo-comments.nvim
       {:dependencies [(use! :nvim-lua/plenary.nvim)]
        :cmd [:TodoTrouble :TodoTelescope]
        :keys [(key! :<leader>ft :<cmd>TodoTelescope<cr> {:desc :TODO})]
        :event :LazyFile
        :opts {:signs false}})
 ;;
 (use! :folke/trouble.nvim {:cmd [:TroubleToggle :Trouble]
                            :keys [(key! :<localleader>t
                                         "<cmd>Trouble diagnostics toggle<cr>"
                                         {:desc :Diagnostics})]
                            ; (key! :<localleader>s
                            ;       "<cmd>Trouble symbols toggle<cr>"
                            ;       {:desc :Symbols})]
                            :opts {:auto_preview false
                                   :use_diagnostic_signs true
                                   :padding false}})]
