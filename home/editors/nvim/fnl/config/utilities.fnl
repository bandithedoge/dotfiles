(import-macros {: load! : trigger! : key!} :config.macros)

(load! :blink.pairs
       {:event [:BufReadPre :BufNewFile]
        :after #(let [blink-pairs (require :blink.pairs)]
                  (blink-pairs.setup {:highlights {:enabled false
                                                   :matchparen {:enabled false}}}))})

(load! :direnv.vim {:event [:BufReadPre :BufNewFile]})

(load! :fold-cycle.nvim
       {:keys [(key! :<tab> #(let [fold-cycle (require :fold-cycle)]
                               (fold-cycle.open)))]
        :after #(let [fold-cycle (require :fold-cycle)]
                  (fold-cycle.setup {:softwrap_movement_fix true}))})

(load! :mkdir.nvim {:event [:BufReadPre :BufNewFile]})

(load! :neogen
       {:cmd :Neogen
        :keys [(key! :<localleader>g :<cmd>Neogen<cr>
                     {:desc "Generate annotation"})]
        :before #(trigger! :nvim-treesitter)
        :after #(let [neogen (require :neogen)]
                  (neogen.setup {:snippet_engine :nvim}))})

(load! :persistence.nvim {:event :BufReadPre})

(load! :presence.nvim {:event [:BufReadPre :BufNewFile]
                       :after #(let [presence (require :presence)]
                                 (presence.setup))})

(load! :remember.nvim {:after #(require :remember)
                       :event [:BufReadPre :BufNewFile]})

(load! :sQVe/sort.nvim {:cmd :Sort})
