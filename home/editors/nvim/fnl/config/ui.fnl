(import-macros {: load! : key!} :config.macros)

(load! :edgy.nvim
       {:event :DeferredUIEnter
        :after #(let [edgy (require :edgy)]
                  (edgy.setup {:wo {:winbar false}
                               :animate {:enabled false}
                               :bottom [:Trouble :dap-repl :dapui_console]
                               :left [:neo-tree]
                               :right [:dapui_scopes]
                               :dapui_breakpoints :dapui_stacks
                               :dapui_watches :Outline}))})

(load! :gitsigns.nvim
       {:event [:BufReadPre :BufNewFile]
        :after #(let [gitsigns (require :gitsigns)]
                  (gitsigns.setup {:signs {:add {:text "▎"}
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
                                   :diff_opts {:internal true}}))})

(load! :nvim-highlight-colors
       {:event [:BufReadPre :BufNewFile]
        :after #(let [highlight-colors (require :nvim-highlight-colors)]
                  (highlight-colors.setup {:enable_tailwind true}))})

(load! :nvim-hlslens
       {:event :CmdlineEnter
        :after #(let [hlslens (require :hlslens)] (hlslens.setup))})

(load! :todo-comments.nvim
       {:keys [(key! :<leader>ft #(_G.Snacks.picker.todo_comments)
                     {:desc :TODO})]
        :event [:BufReadPre :BufNewFile]
        :after #(let [todo-comments (require :todo-comments)]
                  (todo-comments.setup {:signs false
                                        :highlight {:multiline false}}))})

(load! :trouble.nvim
       {:cmd [:TroubleToggle :Trouble]
        :keys [(key! :<localleader>t "<cmd>Trouble diagnostics toggle<cr>"
                     {:desc :Diagnostics})]
        :after #(let [trouble (require :trouble)]
                  (trouble.setup {:auto_preview false
                                  :use_diagnostic_signs true
                                  :padding false}))})
