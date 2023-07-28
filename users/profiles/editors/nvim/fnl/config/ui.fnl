(require-macros :hibiscus.vim)

[;; indent-blankline.nvim {{{
 (_G.use :lukas-reineke/indent-blankline.nvim
         {:dependencies [(_G.use :nvim-treesitter/nvim-treesitter)]
          :event [:BufReadPre :BufNewFile]
          :opts {:show_current_context true
                 :char "│"
                 :use_treesitter true
                 :use_treesitter_scope true
                 :filetype_exclude [:help :TelescopePrompt :neo-tree]
                 :buftype_exclude [:terminal]
                 :show_foldtext false
                 :indent_level 30}})
 ;; }}}
 ;; nvim-colorizer.lua {{{
 (_G.use :NvChad/nvim-colorizer.lua
         {:event [:BufReadPre :BufNewFile]
          :opts {:filetypes ["*"] :user_default_options {:css true}}})
 ;; gitsigns.nvim {{{
 (_G.use :lewis6991/gitsigns.nvim
         {:event [:BufReadPre :BufNewFile] :opts {:diff_opts {:internal true}}})
 ;; FTerm.nvim {{{
 (_G.use :numToStr/FTerm.nvim
         {:keys [(_G.key :<leader>T
                         #(let [fterm (require :FTerm)] (fterm.toggle))
                         {:desc :Terminal})]
          :opts {:hl :NormalFloat :border :solid}})
 ;; }}}
 ;; fm-nvim {{{
 (_G.use :is0n/fm-nvim
         {:keys [(_G.key :<leader>g :<cmd>Lazygit<cr> {:desc :Git})]
          :opts {:ui {:float {:border :solid :float_hl :NormalPopover}}}})
 ;; nvim-hlslens {{{
 (_G.use :kevinhwang91/nvim-hlslens
         {:event [:BufReadPre :BufNewFile] :config true})
 ;; }}}
 ;; pretty-fold.nvim {{{
 (_G.use :anuvyklack/pretty-fold.nvim
         {:event [:BufReadPre :BufNewFile]
          :opts {:fill_char " "
                 :process_comment_signs false
                 :sections {:left [:content] :right [:number_of_folded_lines]}}})
 ;; foldsigns.nvim {{{
 (_G.use :lewis6991/foldsigns.nvim
         {:event [:BufReadPre :BufNewFile] :config true})
 ;; }}}
 ;; todo-comments.nvim {{{
 (_G.use :folke/todo-comments.nvim
         {:dependencies [(_G.use :nvim-lua/plenary.nvim)]
          :event [:BufReadPre :BufNewFile]
          :config true})
 ;; }}}
 ;; neo-tree.nvim {{{
 (_G.use :nvim-neo-tree/neo-tree.nvim
         {:dependencies [(_G.use :nvim-lua/plenary.nvim)
                         (_G.use :nvim-tree/nvim-web-devicons)
                         (_G.use :MunifTanjim/nui.nvim)
                         (_G.use :echasnovski/mini.nvim)]
          :keys [(_G.key :<leader>t "<cmd>Neotree toggle<cr>"
                         {:desc "File tree"})]
          :cmd :Neotree
          :config #(let [neo-tree (require :neo-tree)
                         bufremove (require :mini.bufremove)]
                     (neo-tree.setup {:sources [:buffers :filesystem]
                                      :popup_border_style :solid
                                      :use_default_mappings false
                                      :default_component_configs {:container {:enable_character_fade false}
                                                                  :name {:trailing_slash true
                                                                         :use_git_status_colors false}
                                                                  :git_status {:symbols {:added ""
                                                                                         :deleted ""
                                                                                         :modified ""
                                                                                         :renamed ""
                                                                                         :untracked ""
                                                                                         :ignored ""
                                                                                         :unstaged ""
                                                                                         :staged ""
                                                                                         :conflict "󰘬"}
                                                                               :align :left}
                                                                  :modified {:symbol "󰆓"}}
                                      :window {:width 30
                                               :mappings {:<2-LeftMouse> :open
                                                          :<cr> :open
                                                          :s :open_split
                                                          :v :open_vsplit
                                                          :R :refresh
                                                          :a {1 :add
                                                              :config {:show_path :relative}}
                                                          :d :delete
                                                          :r :rename
                                                          :y :copy_to_clipboard
                                                          :x :cut_to_clipboard
                                                          :p :paste_from_clipboard
                                                          :q :close_window
                                                          :? :show_help}}
                                      :filesystem {:window {:mappings {:H :toggle_hidden
                                                                       :/ :fuzzy_finder
                                                                       :h :navigate_up
                                                                       :l :set_root}}
                                                   :filtered_items {:hide_dotfiles false
                                                                    :hide_gitignored false
                                                                    :hide_hidden false}}
                                      :buffers {:window {:mappings {:d :bdelete
                                                                    :D :bforce}}
                                                :commands {:bdelete #(let [node ($1.tree:get_node)
                                                                           path (node:get_id)]
                                                                       (bufremove.delete node.extra.bufnr))
                                                           :bforce #(let [node ($1.tree:get_node)
                                                                          path (node:get_id)]
                                                                      (bufremove.delete node.extra.bufnr
                                                                                        true))}}}))})
 ;; neodim {{{
 (_G.use :zbirenbaum/neodim {:event :LspAttach :opts {:blend_color _G.base00}})
 ;; }}}
 ;; cybu.nvim {{{
 (_G.use :ghillb/cybu.nvim
         {:keys [(_G.key :<s-tab> :<cmd>CybuLastusedNext<cr>)]
          :opts {:position {:anchor :topcenter}
                 :style {:border :solid}
                 :behavior {:mode {:last_used {:view :paging
                                               :switch :immediate}}}}})]
