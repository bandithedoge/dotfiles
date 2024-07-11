[(_G.use :folke/which-key.nvim
         {:keys [(_G.key :<bs> "<cmd>WhichKey <localleader><cr>")]
          :event :VeryLazy
          :opts {:ignore_missing false
                 :icons {:separator "󰅂"}
                 :show_help false
                 :disable {:filetypes [:TelescopePrompt]}}
          :config #(let [wk (require :which-key)]
                     (wk.setup $2)
                     (wk.register (let [lsp vim.lsp.buf]
                                    {:<leader> {:f {:name :Find}
                                                :o {:name :Open
                                                    :d ["<cmd>cd ~/dotfiles<cr>"
                                                        :Dotfiles]
                                                    :g ["<cmd>cd ~/git<cr>"
                                                        :Git]
                                                    :s ["<cmd>cd ~/sql<cr>"
                                                        :School]}
                                                :W [:<cmd>close<cr>
                                                    "Close window"]
                                                :l [:<cmd>Lazy<cr> :lazy.nvim]}
                                     :<localleader> {:s [lsp.document_symbol
                                                         :Symbols]
                                                     :d {:name :Debug}
                                                     :r [lsp.rename :Rename]
                                                     :l {:name :Language
                                                         :r {:name :Repl}}}})))})]

