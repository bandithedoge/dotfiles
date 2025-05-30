(import-macros {: use! : key!} :config.macros)
(import-macros {: g!} :hibiscus.vim)

[(use! :nvim-treesitter/nvim-treesitter
       {:dependencies [(use! :windwp/nvim-ts-autotag)
                       (use! :folke/ts-comments.nvim {:opts {}})
                       (use! :nvim-treesitter/playground)
                       (use! :HiPhish/rainbow-delimiters.nvim
                             {:config #(let [rainbow-delimiters (require :rainbow-delimiters)]
                                         (g! :rainbow_delimiters
                                             {:strategy {"" rainbow-delimiters.strategy.global}
                                              :highlight [:rainbowcol1
                                                          :rainbowcol2
                                                          :rainbowcol3
                                                          :rainbowcol4
                                                          :rainbowcol5
                                                          :rainbowcol6]}))})
                       (use! :Wansmer/treesj
                             {:keys [(key! :gj
                                           #(let [treesj (require :treesj)]
                                              (treesj.join)))
                                     (key! :gk
                                           #(let [treesj (require :treesj)]
                                              (treesj.split)))]
                              :opts {:use_default_keymaps false}})
                       (use! :calops/hmts.nvim)]
        :build (when (not _G.USING_NIX) ":TSUpdate")
        :event :LazyFile
        :opts {:auto_install (not _G.USING_NIX)
               :highlight {:enable true
                           :additional_vim_regex_highlighting [:org]}
               :incremental_selection {:enable true
                                       :init_selection :<C-Space>
                                       :node_incremental :<C-space>
                                       :scope_incremental false
                                       :node_decremental :<bs>}
               :playground {:enable true}
               :autotag {:enable true :filetypes [:html :xml]}}
        :config #(let [ts (require :nvim-treesitter.configs)]
                   (ts.setup $2))})]
