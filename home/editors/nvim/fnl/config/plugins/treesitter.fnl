(import-macros {: load! : trigger! : key!} :config.macros)
(import-macros {: g!} :hibiscus.vim)

(load! :nvim-ts-autotag {:lazy true})

(load! :ts-comments.nvim
       {:lazy true
        :after #(let [ts-comments (require :ts-comments)] (ts-comments.setup))})

(load! :playground {:lazy true})

(load! :rainbow-delimiters.nvim
       {:lazy true
        :after #(let [rainbow-delimiters (require :rainbow-delimiters)]
                  (g! :rainbow_delimiters
                      {:strategy {"" rainbow-delimiters.strategy.global}
                       :highlight [:rainbowcol1
                                   :rainbowcol2
                                   :rainbowcol3
                                   :rainbowcol4
                                   :rainbowcol5
                                   :rainbowcol6]}))})

(load! :treesj
       {:keys [(key! :gj #(let [treesj (require :treesj)]
                            (treesj.join)))
               (key! :gk #(let [treesj (require :treesj)]
                            (treesj.split)))]
        :after #(let [treesj (require :treesj)]
                  (treesj.setup {:use_default_keymaps false}))})

(load! :hmts.nvim {:lazy true})

(load! :nvim-treesitter/nvim-treesitter
       {:event [:BufReadPre :BufNewFile]
        :before #(do
                   (trigger! :nvim-ts-autotag)
                   (trigger! :ts-comments.nvim)
                   (trigger! :playground)
                   (trigger! :rainbow-delimiters.nvim)
                   (trigger! :treesj)
                   (trigger! :hmts.nvim))
        :after #(let [ts (require :nvim-treesitter.configs)]
                  (ts.setup {:auto_install false
                             :highlight {:enable true
                                         :additional_vim_regex_highlighting [:org]}
                             :incremental_selection {:enable true
                                                     :init_selection :<C-Space>
                                                     :node_incremental :<C-space>
                                                     :scope_incremental false
                                                     :node_decremental :<bs>}
                             :playground {:enable true}
                             :autotag {:enable true :filetypes [:html :xml]}}))})
