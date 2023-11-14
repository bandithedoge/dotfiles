(require-macros :hibiscus.vim)

[(_G.use :gpanders/nvim-parinfer
         {:event [:BufReadPre :BufNewFile]
          :init #(do
                   (g! parinfer_no_maps true)
                   (vim.api.nvim_create_autocmd :User
                                        {:pattern :parinfer
                                         :callback #(do
                                                      (map! [i :buffer] :<Tab>
                                                            "<Plug>(parinfer-tab)")
                                                      (map! [i :buffer]
                                                            :<S-Tab>
                                                            "<Plug>(parinfer-backtab)"))}))})
 ;;
 (_G.use :madskjeldgaard/faust-nvim
         {:ft :faust
          :config #(let [faust (require :faust)]
                     (faust.load_snippets))})
 ;;
 (_G.use :vmchale/dhall-vim)
 ;;
 (_G.use :milisims/nvim-luaref)
 ;;
 (_G.use :purescript-contrib/purescript-vim)
 ;;
 (_G.use :Fymyte/rasi.vim)
 ;;
 (_G.use :kchmck/vim-coffee-script)
 ;;
 (_G.use :gmoe/vim-faust)
 ;;
 (_G.use :elkowar/yuck.vim)
 ;;
 (_G.use :luckasRanarison/tree-sitter-hypr)]
