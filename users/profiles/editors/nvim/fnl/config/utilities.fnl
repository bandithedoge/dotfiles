(require-macros :hibiscus.vim)
[;; mini.nvim {{{
 (_G.use :echasnovski/mini.nvim
         {:cmd [:Bufremove :BufremoveForce]
          :lazy false
          :keys [(_G.key :<leader>w
                         "<cmd>lua require('mini.bufremove').delete()<cr>"
                         {:desc "Close buffer"})
                 (_G.key :<leader><C-w>
                         "<cmd>lua require('mini.bufremove').delete(0, true)<cr>"
                         {:desc "Close buffer (force)"})]
          :init #(let [bufremove (require :mini.bufremove)]
                   (bufremove.setup))
          :setup #(let [surround (require :mini.surround)
                        trailspace (require :mini.trailspace)]
                    (surround.setup {})
                    (trailspace.setup {}))})
 ;; }}}
 ;; Comment.nvim {{{
 (_G.use :numToStr/Comment.nvim)
 ;; }}}
 ;; sort.nvim {{{
 (_G.use :sQVe/sort.nvim {:cmd :Sort})
 ;; }}}
 ;; nvim-autopairs {{{
 (_G.use :windwp/nvim-autopairs
         {:dependencies [(_G.use :hrsh7th/nvim-cmp)]
          :event :InsertEnter
          :opts {:check_ts true :enable_check_bracket_line true}
          :config #(let [autopairs (require :nvim-autopairs)
                         cmp (require :cmp)
                         cmp-autopairs (require :nvim-autopairs.completion.cmp)]
                     (autopairs.setup $2)
                     (cmp.event:on :confirm_done
                                   (cmp-autopairs.on_confirm_done {:map_char {:tex ""}})))})
 ;; }}}
 ;; neogen {{{
 (_G.use :danymat/neogen
         {:dependencies [(_G.use :nvim-treesitter/nvim-treesitter)]
          :cmd :Neogen
          :keys [(_G.key :<localleader>g :<cmd>Neogen<cr>
                         {:desc "Generate annotation"})]
          :opts {:snippet_engine :luasnip}})
 ;; }}}
 ;; icon-picker.nvim {{{
 (_G.use :ziontee113/icon-picker.nvim
         {:dependencies [(_G.use :nvim-telescope/telescope.nvim)]
          :cmd [:IconPickerNormal :IconPickerInsert :IconPickerYank]
          :keys [(_G.key :<leader>i
                         "<cmd>:IconPickerNormal alt_font emoji nerd_font nerd_font_v3 symbols<cr>"
                         {:desc "Insert symbol"})]
          :config true})
 ;; }}}
 ;; remember.nvim {{{
 (_G.use :vladdoster/remember.nvim {:config #(require :remember)})
 ;; }}}
 ;; yanky.nvim {{{
 (_G.use :gbprod/yanky.nvim
         {:config #(let [yanky (require :yanky)]
                     (yanky.setup))})
 ;; }}}
 ;; fold-cycle.nvim {{{
 (_G.use :jghauser/fold-cycle.nvim
         {:keys [(_G.key :<tab>
                         #(let [fold-cycle (require :fold-cycle)]
                            (fold-cycle.open)))]
          :opts {:softwrap_movement_fix true}})
 ;; }}}
 ;; nvim-expand-expr {{{
 (_G.use :Allendang/nvim-expand-expr
         {:keys [(_G.key :<localleader>e
                         #(let [expand-expr (require :expand_expr)]
                            (expand-expr.expand))
                         {:desc "Expand expression"})]})
 ;; }}}
 (_G.use :direnv/direnv.vim)
 (_G.use :gpanders/editorconfig.nvim)
 (_G.use :jghauser/mkdir.nvim)
 (_G.use :andweeb/presence.nvim)]
