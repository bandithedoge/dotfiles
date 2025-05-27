(import-macros {: use! : key!} :config.macros)
(import-macros {: g! : map!} :hibiscus.vim)

[(use! :folke/lazydev.nvim {:ft :lua :config true})
 ;;
 (use! :milisims/nvim-luaref)
 ;;
 (use! :gpanders/nvim-parinfer
       {:event [:BufReadPre :BufNewFile]
        :init #(do
                 (g! :parinfer_force_balance true)
                 (vim.api.nvim_create_autocmd :User
                                              {:pattern :parinfer
                                               :callback #(do
                                                            (map! [:i :buffer]
                                                                  :<Tab>
                                                                  "<Plug>(parinfer-tab)")
                                                            (map! [:i :buffer]
                                                                  :<S-Tab>
                                                                  "<Plug>(parinfer-backtab)"))}))})
 ;;
 (let [ft :typst]
   (use! :chomosuke/typst-preview.nvim
         {: ft
          :keys [(key! :<localleader>lp :<cmd>TypstPreview<cr>
                       {:desc :Preview : ft})]
          :opts {}}))
 ;;
 (use! :NoahTheDuke/vim-just)
 ;;
 (use! :elkowar/yuck.vim)]
