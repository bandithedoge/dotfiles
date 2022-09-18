(require-macros :hibiscus.vim)

;; indent-blankline.nvim {{{
(let [indent_blankline (require :indent_blankline)]
  (indent_blankline.setup {:show_current_context true
                           :char "│"
                           :use_treesitter true
                           :filetype_exclude [:help :TelescopePrompt :neo-tree]
                           :buftype_exclude [:terminal]
                           :show_foldtext false
                           :indent_level 30}))

;; }}}

;; nvim-colorizer.lua {{{
(let [colorizer (require :colorizer)]
  (colorizer.setup {1 "*"
                    :DEFAULT_OPTIONS {:RRGGBBAA true :css true :css_fn true}}))

;; }}}

;; gitsigns.nvim {{{
(let [gitsigns (require :gitsigns)]
  (gitsigns.setup {:diff_opts {:internal true}}))

;; }}}

;; FTerm.nvim {{{
(let [fterm (require :FTerm)]
  (set _G.fterm_float (fterm:new {:hl :NormalFloat :border :solid})))

;; }}}

;; mini.nvim {{{
(let [trailspace (require :mini.trailspace)]
  (trailspace.setup))

;; }}}

;; fm-nvim {{{
(let [fm-nvim (require :fm-nvim)]
  (fm-nvim.setup {:ui {:float {:border :solid :float_hl :NormalPopover}}}))

;; nvim-hlslens {{{
(let [hlslens (require :hlslens)]
  (hlslens.setup))

;; }}}

;; pretty-fold.nvim {{{
(let [pretty-fold (require :pretty-fold)]
  (pretty-fold.setup {:fill_char " "
                      :process_comment_signs false
                      :sections {:left [:content]
                                 :right [:number_of_folded_lines]}}))

;; }}}

;; dressing.nvim {{{
(let [dressing (require :dressing)
      themes (require :telescope.themes)]
  (dressing.setup {:input {:border :solid :winblend 0}
                   :select {:backend [:telescope :nui :builtin]
                            :telescope (themes.get_cursor {:border true
                                                           :borderchars [" "
                                                                         " "
                                                                         " "
                                                                         " "
                                                                         " "
                                                                         " "
                                                                         " "
                                                                         " "]})}}))

;; }}}

;; cinnamon.nvim {{{
(let [cinnamon (require :cinnamon)]
  (cinnamon.setup {:extra_keymaps true :default_delay 3 :centered false}))

;; }}}

;; flare.nvim {{{
(let [flare (require :flare)]
  (flare.setup {:hl_group :PmenuSel}))

;; }}}

;; foldsigns.nvim {{{
(let [foldsigns (require :foldsigns)]
  (foldsigns.setup))

;; }}}

;; todo-comments.nvim {{{
(let [todo-comments (require :todo-comments)]
  (todo-comments.setup {}))

;; }}}

;; trouble.nvim {{{
(let [trouble (require :trouble)]
  (trouble.setup {:auto_preview false}))

;; }}}

;; hlargs.nvim {{{
(let [hlargs (require :hlargs)]
  (hlargs.setup))

;; }}}

;; hover.nvim {{{
(let [hover (require :hover)]
  (hover.setup {:init #(do
                         (require :hover.providers.lsp)
                         (require :hover.providers.gh)
                         (require :hover.providers.man))
                :preview_opts {:border :solid}})
  (map! [n] :K hover.hover)
  (map! [n] :gK hover.hover_select))

;; }}}

;; neo-tree.nvim {{{
(let [neo-tree (require :neo-tree)
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
                                                                      :untracked ""
                                                                      :ignored ""
                                                                      :unstaged "ﱡ"
                                                                      :staged ""
                                                                      :conflict ""}
                                                            :align :left}
                                               :modified {:symbol "ﱣ"}}
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
                                                 :hide_gitignored false}}
                   :buffers {:window {:mappings {:d :bdelete :D :bforce}}
                             :commands {:bdelete #(let [node ($1.tree:get_node)
                                                        path (node:get_id)]
                                                    (bufremove.delete node.extra.bufnr))
                                        :bforce #(let [node ($1.tree:get_node)
                                                       path (node:get_id)]
                                                   (bufremove.delete node.extra.bufnr
                                                                     true))}}}))

;; neodim {{{
(let [neodim (require :neodim)]
  (neodim.setup {:blend_color _G.base00}))

;; }}}

;; fidget.nvim {{{
(let [fidget (require :fidget)]
  (fidget.setup {:text {:spinner :dots}}))

;; }}}
