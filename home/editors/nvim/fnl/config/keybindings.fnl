(import-macros {: load! : key! : tx!} :config.macros)

(load! :which-key.nvim
       {:keys [(key! :<bs> "<cmd>WhichKey <localleader><cr>" {:mode [:n :v]})]
        :event :DeferredUIEnter
        :after #(let [wk (require :which-key)]
                  (wk.setup {:delay 0
                             :icons {:separator "󰅂" :mappings false}
                             :show_help false
                             :spec [(tx! :<leader>W :<cmd>close<cr>
                                         {:desc "Close window"})
                                    (tx! :<leader>f {:group :Find})
                                    (tx! :<leader>l :<cmd>Lazy<cr>
                                         {:desc :lazy.nvim})
                                    (tx! :<leader>o {:group :Open})
                                    (tx! :<leader>od "<cmd>cd ~/dotfiles<cr>"
                                         {:desc :Dotfiles})
                                    (tx! :<leader>og "<cmd>cd ~/git<cr>"
                                         {:desc :Git})
                                    (tx! :<localleader>d {:group :Debug})
                                    (tx! :<localleader>l
                                         {:group :Language-specific})
                                    (tx! :<localleader>r vim.lsp.buf.rename
                                         {:desc :Rename})
                                    (tx! :<localleder>lr {:name :REPL})]}))})

; [(use! :folke/which-key.nvim
;        {:keys [(key! :<bs> "<cmd>WhichKey <localleader><cr>" {:mode [:n :v]})]
;         :event :VeryLazy
;         :opts {:delay 0
;                :icons {:separator "󰅂" :mappings false}
;                :show_help false
;                :spec [(tx! :<leader>W :<cmd>close<cr> {:desc "Close window"})
;                       (tx! :<leader>f {:group :Find})
;                       (tx! :<leader>l :<cmd>Lazy<cr> {:desc :lazy.nvim})
;                       (tx! :<leader>o {:group :Open})
;                       (tx! :<leader>od "<cmd>cd ~/dotfiles<cr>"
;                            {:desc :Dotfiles})
;                       (tx! :<leader>og "<cmd>cd ~/git<cr>" {:desc :Git})
;                       (tx! :<localleader>d {:group :Debug})
;                       (tx! :<localleader>l {:group :Language-specific})
;                       (tx! :<localleader>r vim.lsp.buf.rename {:desc :Rename})
;                       (tx! :<localleder>lr {:name :REPL})]}})]
