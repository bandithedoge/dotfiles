[(_G.use :LunarVim/bigfile.nvim {:event :BufReadPre})
 ;;
 (_G.use :direnv/direnv.vim {:cond _G.USING_NIX :event :LazyFile})
 ;;
 (_G.use :jghauser/fold-cycle.nvim
         {:keys [(_G.key :<tab>
                         #(let [fold-cycle (require :fold-cycle)]
                            (fold-cycle.open)))]
          :opts {:softwrap_movement_fix true}})
 ;;
 (_G.use :3rd/image.nvim {:ft [:markdown :norg :typst] :opts {}})
 ;;
 (_G.use :jghauser/mkdir.nvim {:event :LazyFile})
 ;;
 (_G.use :danymat/neogen
         {:dependencies [(_G.use :nvim-treesitter/nvim-treesitter)]
          :cmd :Neogen
          :keys [(_G.key :<localleader>g :<cmd>Neogen<cr>
                         {:desc "Generate annotation"})]
          :opts {:snippet_engine :luasnip}})
 ;;
 (_G.use :figsoda/nix-develop.nvim {:cmd [:NixDevelop :NixShell :RiffShell]})
 ;;
 (_G.use :Allendang/nvim-expand-expr
         {:keys [(_G.key :<localleader>e
                         #(let [expand-expr (require :expand_expr)]
                            (expand-expr.expand))
                         {:desc "Expand expression"})]})
 ;;
 (_G.use :andweeb/presence.nvim {:event :LazyFile :opts {}})
 ;;
 (_G.use :vladdoster/remember.nvim
         {:config #(require :remember) :event :LazyFile})
 ;;
 (_G.use :sQVe/sort.nvim {:cmd :Sort})
 ;;
 (_G.use :gbprod/yanky.nvim
         {:config true
          :event :LazyFile
          :keys [(_G.key :y "<Plug>(YankyYank)" {:mode [:n :x]})
                 (_G.key :p "<Plug>(YankyPutAfter)" {:mode [:n :x]})
                 (_G.key :P "<Plug>(YankyPutBefore)" {:mode [:n :x]})]})]
