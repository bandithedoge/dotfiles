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
                    :diagnostics {:enable true :show_on_dirs true}
                    :view {:mappings {:list [{:key :h :cb (cb :dir_up)}
                                             {:key :l :cb (cb :cd)}]}
                           :width 30}
                    :renderer {:indent_markers {:enable true}
                               :icons {:git_placement :after}}
                    :filters {:dotfiles false :custom [:.DS_Store]}}))

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

;; fidget.nvim {{{
(let [fidget (require :fidget)]
  (fidget.setup {:text {:spinner :dots} :window {:blend 0}}))

;; }}}

;; dressing.nvim {{{
(let [dressing (require :dressing)
      themes (require :telescope.themes)]
  (dressing.setup {:input {:border :solid :winblend 0}
                   :select {:backend [:telescope :nui :builtin]
                            :telescope (themes.get_cursor)}}))

;; }}}

;; cinnamon.nvim {{{
(let [cinnamon (require :cinnamon)]
  (cinnamon.setup {:extra_keymaps true :default_delay 3 :centered false}))

;; }}}

;; e-kaput.nvim {{{
(let [e-kaput (require :e-kaput)]
  (e-kaput.setup {:borders false :transparency 0}))

(vim.diagnostic.config {:virtual_text false})

;; }}}

;; flare.nvim {{{
(let [flare (require :flare)]
  (flare.setup {:hl_group :PmenuSel}))

;; }}}

;; foldsigns.nvim {{{
(let [foldsigns (require :foldsigns)]
  (foldsigns.setup))

;; }}}

;; nvim-notify {{{
(let [notify (require :notify)
      spinner-frames ["⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷"]]
  (notify.setup {:fps 60})
  (set vim.notify notify))

;; }}}

;; satellite.nvim {{{
(let [satellite (require :satellite)]
  (satellite.setup {:width 1 :current_only true}))

;; }}}

;; todo-comments.nvim {{{
(let [todo-comments (require :todo-comments)]
  (todo-comments.setup {}))

;; }}}

;; trouble.nvim {{{
(let [trouble (require :trouble)]
  (trouble.setup {:auto_preview false}))
;; }}}
