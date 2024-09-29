(import-macros {: merge!} :hibiscus.core)

[(_G.use :folke/which-key.nvim
         {:keys [(_G.key :<bs> "<cmd>WhichKey <localleader><cr>" {:mode [:n :v]})]
          :event :VeryLazy
          :opts {:delay 0
                 :icons {:separator "󰅂" :mappings false}
                 :show_help false
                 :spec [(merge! [:<leader>W :<cmd>close<cr>]
                                {:desc "Close window"})
                        (merge! [:<leader>f] {:group :Find})
                        (merge! [:<leader>l :<cmd>Lazy<cr>] {:desc :lazy.nvim})
                        (merge! [:<leader>o] {:group :Open})
                        (merge! [:<leader>od "<cmd>cd ~/dotfiles<cr>"]
                                {:desc :Dotfiles})
                        (merge! [:<leader>og "<cmd>cd ~/git<cr>"] {:desc :Git})
                        (merge! [:<localleader>d] {:group :Debug})
                        (merge! [:<localleader>l] {:group :Language-specific})
                        (merge! [:<localleader>r vim.lsp.buf.rename]
                                {:desc :Rename})
                        (merge! [:<localleader>s vim.lsp.buf.document_symbol]
                                {:desc :Symbols})
                        (merge! [:<localleder>lr] {:name :REPL})]}})]

