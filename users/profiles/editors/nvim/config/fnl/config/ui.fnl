(module config.ui)

;; indent-blankline.nvim {{{
(let [indent_blankline (require :indent_blankline)]
  (indent_blankline.setup {:show_current_context true
                           :char "│"
                           :use_treesitter true
                           :filetype_exclude [:help :TelescopePrompt]
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

;; nvim-tree.lua {{{
(let [nvim-tree (require :nvim-tree)
      config (require :nvim-tree.config)
      cb config.nvim_tree_callback]
  (nvim-tree.setup {:hijack_cursor true
                    :hijack_netrw true
                    :update_cwd true
                    :update_focused_file {:enable true}
                    :diagnostics {:enable true :show_on_dirs true}
                    :view {:auto_resize true
                           :hide_root_folder true
                           :mappings {:list [{:key :h :cb (cb :dir_up)}
                                             {:key :l :cb (cb :cd)}]}}
                    :filters {:dotfiles false
                              :custom [:.DS_Store
                                       :.git
                                       :node_modules
                                       :__pycache__]}}))

(set vim.g.nvim_tree_indent_markers 1)
(set vim.g.nvim_tree_show_icons {:git 1 :folders 1 :files 1 :folder_arrows 0})
(set vim.g.nvim_tree_add_trailing 1)

;; }}}

;; FTerm.nvim {{{
(let [fterm (require :FTerm)]
  (global fterm_float (fterm:new {:hl :NormalPopover :border :solid})))

;; }}}

;; mini.nvim {{{
(let [trailspace (require :mini.trailspace)
      cursorword (require :mini.cursorword)]
  (trailspace.setup)
  (cursorword.setup))

;; }}}

;; fm-nvim {{{
(let [fm-nvim (require :fm-nvim)]
  (fm-nvim.setup {:ui {:float {:border :solid :float_hl :NormalPopover}
                       ; don't fuck with my folds :(
                       }}))

;; }}}

;; nvim-hlslens {{{
(let [hlslens (require :hlslens)]
  (hlslens.setup))

;; }}}

;; specs.nvim {{{
(let [specs (require :specs)]
  (specs.setup {:popup {:winhl :PmenuSel :fader specs.exp_fader}}))

;; }}}

;; pretty-fold.nvim {{{
(let [pretty-fold (require :pretty-fold)]
  (pretty-fold.setup {:fill_char " "
                      :process_comment_signs false
                      :sections {:left [:content]
                                 :right [:number_of_folded_lines]}}))

;; }}}

;; fidget.nvim {{{
(let [fidget (require :fidget)]
  (fidget.setup {:text {:spinner :dots}}))

;; }}}

;; cheatsheet.nvim {{{
(let [cheatsheet (require :cheatsheet)
      actions (require :cheatsheet.telescope.actions)]
  (cheatsheet.setup {:telescope_mappings {:<CR> actions.select_or_execute}}))

;; }}}

;; dressing.nvim {{{
(let [dressing (require :dressing)
      themes (require :telescope.themes)]
  (dressing.setup {:input {:border :solid}
                   :select {:backend [:telescope :nui :builtin]
                            :telescope (themes.get_cursor)}}))
