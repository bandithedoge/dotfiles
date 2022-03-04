;; lualine {{{
(let [lualine (require :lualine)
      gps (require :nvim-gps)]
  (gps.setup {:separator "  "})
  (lualine.setup {:options {:theme :nightfox
                            :component_separators ""
                            :section_separators ""}
                  :extensions [:nvim-tree]
                  :sections {:lualine_a [:mode]
                             :lualine_b [{1 :filename :path 0}]
                             :lualine_c [{1 :diagnostics
                                          :sources [:nvim_diagnostic]
                                          :update_in_insert true}
                                         {1 gps.get_location
                                          :cond gps.is_available}]
                             :lualine_x [:diff]
                             :lualine_y [[(lambda []
                                            (.. " " vim.bo.shiftwidth))]
                                         :fileformat
                                         :filetype]
                             :lualine_z [:location]}}))

;; }}}

;; indent-blankline.nvim {{{
(let [indent_blankline (require :indent_blankline)]
  (indent_blankline.setup {:show_current_context true
                           :char "│"
                           :use_treesitter true
                           :filetype_exclude [:help :TelescopePrompt]
                           :buftype_exclude [:terminal]
                           :show_foldtext false}))

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
                           :mappings {:list [{:key :h :cb (cb :dir_up)}
                                             {:key :l :cb (cb :cd)}]}}
                    :filters {:dotfiles false
                              :custom [:.DS_Store
                                       :.git
                                       :node_modules
                                       :__pycache__]}}))

(set vim.g.nvim_tree_indent_markers 1)
(set vim.g.nvim_tree_show_icons {:git 1 :folders 1 :files 1 :folder_arrows 0})

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

;; lsp_lines.nvim {{{
(let [lsp_lines (require :lsp_lines)]
  (lsp_lines.register_lsp_virtual_lines))

(vim.diagnostic.config {:virtual_text false})

;; }}}

;; cheatsheet.nvim {{{
(let [cheatsheet (require :cheatsheet)
      actions (require :cheatsheet.telescope.actions)]
  (cheatsheet.setup {:telescope_mappings {:<CR> actions.select_or_execute}}))

;; }}}

;; dressing.nvim {{{
(let [dressing (require :dressing)]
  (dressing.setup {:input {:border :solid}
                   :select {:backend [:telescope :nui :builtin]
                            :telescope {:theme :cursor}
                            ;
                            }}))

;; }}}

;; nvim-cokeline {{{
(let [cokeline (require :cokeline)
      utils (require :cokeline.utils)]
  (cokeline.setup {:default_hl {:focused {:fg (utils.get_hex :Normal :bg)
                                          :bg (utils.get_hex :FloatBorder :fg)}
                                :unfocused {:fg (utils.get_hex :Comment :fg)
                                            :bg (utils.get_hex :StatusLine :bg)}}
                   :components [{:text (lambda [buffer]
                                         (.. " " buffer.devicon.icon))
                                 :hl {}
                                 :truncation {:priority 1}}
                                {:text (lambda [buffer]
                                         buffer.unique_prefix)
                                 :hl {:style :italic}
                                 :truncation {:priority 3 :direction :left}}
                                {:text (lambda [buffer]
                                         (.. buffer.filename " "))}
                                {:text (lambda [buffer]
                                         (or (and (not= buffer.diagnostics.errors
                                                        0)
                                                  (.. " "
                                                      buffer.diagnostics.errors
                                                      " "))
                                             (and (not= buffer.diagnostics.warnings
                                                        0)
                                                  (.. " "
                                                      buffer.diagnostics.warnings
                                                      " "))
                                             ""))}
                                {:text (lambda [buffer]
                                         (or (and buffer.is_modified " ") ""))}]}))

;; }}}
