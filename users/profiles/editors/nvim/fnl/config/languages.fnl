[;; faust-nvim {{{
 (_G.use :madskjeldgaard/faust-nvim
         {:ft :faust
          :config #(let [faust (require :faust)]
                     (faust.load_snippets))})
 ;; }}}
 (_G.use :gpanders/nvim-parinfer)
 (_G.use :vmchale/dhall-vim)
 (_G.use :alaviss/nim.nvim)
 (_G.use :milisims/nvim-luaref)
 (_G.use :purescript-contrib/purescript-vim)
 (_G.use :Fymyte/rasi.vim)
 (_G.use :kchmck/vim-coffee-script)
 (_G.use :gmoe/vim-faust)
 (_G.use :elkowar/yuck.vim)]
