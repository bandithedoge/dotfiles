(require-macros :hibiscus.vim)

[(_G.use :nvim-treesitter/nvim-treesitter
         {:dependencies [(_G.use "HiPhish/rainbow-delimiters.nvim"
                                 {:config #(let [rainbow-delimiters (require :rainbow-delimiters)]
                                             (g! :rainbow_delimiters {:strategy {"" rainbow-delimiters.strategy.global}
                                                                      :highlight [:rainbowcol1
                                                                                  :rainbowcol2
                                                                                  :rainbowcol3
                                                                                  :rainbowcol4
                                                                                  :rainbowcol5
                                                                                  :rainbowcol6
                                                                                  :rainbowcol7]}))})
                         (_G.use :windwp/nvim-ts-autotag)
                         (_G.use :JoosepAlviste/nvim-ts-context-commentstring)
                         (_G.use :nvim-treesitter/playground)]
          :build (when (not _G.USING_NIX) ":TSUpdate")
          :event [:BufReadPost :BufNewFile]
          :opts {:auto_install (not _G.USING_NIX)
                 :highlight {:enable true}
                 :indent {:enable true}
                 :playground {:enable true}
                 :autotag {:enable true :filetypes [:html :xml]}
                 :context_commentstring {:enable true}}
          :config #(let [ts (require :nvim-treesitter.configs)]
                    (ts.setup $2))})]
