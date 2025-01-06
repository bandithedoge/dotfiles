(import-macros {: use! : key!} :config.macros)
(import-macros {: g! : map!} :hibiscus.vim)

[(let [ft [:c :cpp :objc :objcpp :cuda :proto]]
   (use! :p00f/clangd_extensions.nvim
         {:lazy true
          :cmd [:ClangdSwitchSourceHeader :ClangdTypeHierarchy]
          : ft
          :keys [(key! :<localleader>lh :<cmd>ClangdSwitchSourceHeader<cr>
                       {:desc "Switch source/header" : ft})
                 (key! :<localleader>lt :<cmd>ClangdTypeHierarchy<cr>
                       {:desc "Type hierarchy" : ft})]}))
 ;;
 (use! :vmchale/dhall-vim)
 ;;
 (use! :madskjeldgaard/faust-nvim
       {:ft :faust
        :config #(let [faust (require :faust)]
                   (faust.load_snippets))})
 ;;
 (use! :akinsho/flutter-tools.nvim {:lazy true})
 ;;
 (let [ft [:haskell :lhaskell :cabal :cabalproject]]
   (use! :MrcJkb/haskell-tools.nvim
         {:lazy true
          :dependencies [(use! :nvim-telescope/telescope.nvim)]
          : ft
          :keys [(key! :<localleader>lh
                       #(let [haskell-tools (require :haskell-tools)]
                          (haskell-tools.hoogle.hoogle_signature))
                       {:desc :Hoogle : ft})
                 (key! :<localleader>lrr
                       #(let [haskell-tools (require :haskell-tools)]
                          (haskell-tools.repl.toggle))
                       {:desc :REPL : ft})
                 (key! :<localleader>lrb
                       #(let [haskell-tools (require :haskell-tools)]
                          (haskell-tools.repl.toggle (vim.api.nvim_buf_get_name 0)))
                       {:desc "REPL (buffer)" : ft})
                 (key! :<localleader>lrq
                       #(let [haskell-tools (require :haskell-tools)]
                          (haskell-tools.repl.quit))
                       {:desc :Quit : ft})]
          :init #(set vim.g.haskell_tools
                      {:hls {:default_settings {:haskell {:formattingProvider :fourmolu}}}})
          :config #(let [telescope (require :telescope)]
                     (telescope.load_extension :ht))}))
 ;;
 (use! :folke/lazydev.nvim {:ft :lua :config true})
 ;;
 (use! :barreiroleo/ltex_extra.nvim {:lazy true :main :ltex_extra})
 ;;
 (use! :milisims/nvim-luaref)
 ;;
 (use! :gpanders/nvim-parinfer
       {:event [:BufReadPre :BufNewFile]
        :init #(do
                 ; (g! parinfer_no_maps true)
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
 (use! :nvim-orgmode/orgmode
       {:ft [:org]
        :opts {:mappings {:prefix :<localleader>l
                          :global {:org_agenda false :org_capture false}}
               :win_border :solid
               :org_highlight_latex_and_related :entities
               :org_startup_indented true
               :org_custom_exports (let [mk-pandoc (lambda [format]
                                                     (lambda [exporter]
                                                       (let [current-file (vim.api.nvim_buf_get_name 0)
                                                             target (.. (vim.fn.fnamemodify current-file
                                                                                            ":p:r")
                                                                        "."
                                                                        format)
                                                             command [:pandoc
                                                                      current-file
                                                                      :-o
                                                                      target]
                                                             on-success #(vim.api.nvim_echo [[(table.concat $1
                                                                                                            "\n")]]
                                                                                            true
                                                                                            {})
                                                             on-error #(vim.api.nvim_echo [[(table.concat $1
                                                                                                          "\n")
                                                                                            :ErrorMsg]]
                                                                                          true
                                                                                          {})]
                                                         (exporter command
                                                                   target
                                                                   on-success
                                                                   on-error))))]
                                     {:d (mk-pandoc :docx)
                                      :h (mk-pandoc :html)
                                      :o (mk-pandoc :odt)})}})
 ;;
 (use! :purescript-contrib/purescript-vim)
 ;;
 (use! :Fymyte/rasi.vim)
 ;;
 (let [ft [:markdown :norg :rmd :org]]
   (use! :MeanderingProgrammer/render-markdown.nvim
         {: ft
          :keys [(key! :<localleader>lm
                       #(let [render-markdown (require :render-markdown)]
                          (render-markdown.toggle))
                       {:desc "Toggle Markdown rendering" : ft})]
          :opts {:file_types ft
                 :win_options {:showbreak {:default "" :rendered "  "}
                               :breakindent {:default false :rendered true}
                               :breakindentopt {:default "" :rendered ""}}
                 :quote {:repeat_linebreak true}
                 :code {:width :block :left_pad 2 :right_pad 4}
                 :heading {:width :block :left_pad 2 :right_pad 4}
                 :indent {:enabled true :skip_heading true}
                 :sign {:enabled false}}}))
 ;;
 (use! :b0o/SchemaStore.nvim {:lazy true})
 ;;
 (use! :luckasRanarison/tree-sitter-hypr)
 ;;
 (use! :kchmck/vim-coffee-script)
 ;;
 (use! :gmoe/vim-faust)
 ;;
 (use! :NoahTheDuke/vim-just)
 ;;
 (use! :elkowar/yuck.vim)]
