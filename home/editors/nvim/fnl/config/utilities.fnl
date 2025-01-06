(import-macros {: use! : key!} :config.macros)

[(use! :direnv/direnv.vim {:cond _G.USING_NIX :event :LazyFile})
 ;;
 (use! :jghauser/fold-cycle.nvim
       {:keys [(key! :<tab> #(let [fold-cycle (require :fold-cycle)]
                               (fold-cycle.open)))]
        :opts {:softwrap_movement_fix true}})
 ;;
 (use! :jghauser/mkdir.nvim {:event :LazyFile})
 ;;
 (use! :danymat/neogen {:dependencies [(use! :nvim-treesitter/nvim-treesitter)]
                        :cmd :Neogen
                        :keys [(key! :<localleader>g :<cmd>Neogen<cr>
                                     {:desc "Generate annotation"})]
                        :opts {:snippet_engine :nvim}})
 ;;
 (use! :figsoda/nix-develop.nvim {:cmd [:NixDevelop :NixShell :RiffShell]})
 ;;
 (use! :Allendang/nvim-expand-expr
       {:keys [(key! :<localleader>e
                     #(let [expand-expr (require :expand_expr)]
                        (expand-expr.expand))
                     {:desc "Expand expression"})]})
 ;;
 (use! :folke/persistence.nvim {:event :BufReadPre})
 ;;
 (use! :andweeb/presence.nvim {:event :LazyFile :opts {}})
 ;;
 (use! :vladdoster/remember.nvim
       {:config #(require :remember) :event :LazyFile})
 ;;
 (use! :sQVe/sort.nvim {:cmd :Sort})]
