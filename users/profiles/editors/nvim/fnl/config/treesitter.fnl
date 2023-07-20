[(_G.use :nvim-treesitter/nvim-treesitter
         {:dependencies [(_G.use :HiPhish/nvim-ts-rainbow2 {:main :ts-rainbow}
                                 :/nvim-ts-rainbow)
                         (_G.use :windwp/nvim-ts-autotag)
                         (_G.use :JoosepAlviste/nvim-ts-context-commentstring)
                         (_G.use :nvim-treesitter/playground)]
          :config #(let [ts (require :nvim-treesitter.configs)
                         rainbow (require :ts-rainbow)]
                     (ts.setup {:highlight {:enable true}
                                :indent {:enable true}
                                :rainbow {:enable true
                                          :strategy rainbow.strategy.global}
                                :playground {:enable true}
                                :autotag {:enable true :filetypes [:html :xml]}
                                :context_commentstring {:enable true}}))})]
