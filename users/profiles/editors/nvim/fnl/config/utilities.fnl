(require-macros :hibiscus.vim)
[(_G.use :sQVe/sort.nvim {:cmd :Sort})
 ;;
 (_G.use :danymat/neogen
         {:dependencies [(_G.use :nvim-treesitter/nvim-treesitter)]
          :cmd :Neogen
          :keys [(_G.key :<localleader>g :<cmd>Neogen<cr>
                         {:desc "Generate annotation"})]
          :opts {:snippet_engine :luasnip}})
 ;;
 (_G.use :ziontee113/icon-picker.nvim
         {:dependencies [(_G.use :nvim-telescope/telescope.nvim)]
          :cmd [:IconPickerNormal :IconPickerInsert :IconPickerYank]
          :keys [(_G.key :<leader>i
                         "<cmd>:IconPickerNormal alt_font emoji nerd_font nerd_font_v3 symbols<cr>"
                         {:desc "Insert symbol"})]
          :config true})
 ;;
 (_G.use :vladdoster/remember.nvim {:config #(require :remember)})
 ;;
 (_G.use :gbprod/yanky.nvim {:config true})
 ;;
 (_G.use :jghauser/fold-cycle.nvim
         {:keys [(_G.key :<tab>
                         #(let [fold-cycle (require :fold-cycle)]
                            (fold-cycle.open)))]
          :opts {:softwrap_movement_fix true}})
 ;;
 (_G.use :Allendang/nvim-expand-expr
         {:keys [(_G.key :<localleader>e
                         #(let [expand-expr (require :expand_expr)]
                            (expand-expr.expand))
                         {:desc "Expand expression"})]})
 ;;
 (_G.use :direnv/direnv.vim {:cond _G.USING_NIX})
 ;;
 (_G.use :gpanders/editorconfig.nvim)
 ;;
 (_G.use :jghauser/mkdir.nvim)
 ;;
 (_G.use :andweeb/presence.nvim)]
