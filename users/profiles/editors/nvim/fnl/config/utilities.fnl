;; mini.nvim {{{
(let [pairs (require :mini.pairs)
      surround (require :mini.surround)
      bufremove (require :mini.bufremove)]
  (surround.setup {})
  (bufremove.setup {}))

;; }}}

;; Comment.nvim {{{
(let [Comment (require :Comment)]
  (Comment.setup {}))

;; }}}

;; sort.nvim {{{
(let [sort (require :sort)]
  (sort.setup {}))

;; }}}

;; nvim-autopairs {{{
(let [autopairs (require :nvim-autopairs)
      cmp (require :cmp)
      cmp_autopairs (require :nvim-autopairs.completion.cmp)]
  (autopairs.setup {:check_ts true :enable_check_bracket_line true})
  (cmp.event:on :confirm_done
                (cmp_autopairs.on_confirm_done {:map_char {:tex ""}})))

;; }}}

;; neogen {{{
(let [neogen (require :neogen)]
  (neogen.setup {:snippet_engine :luasnip}))

;;}}}

;; icon-picker.nvim {{{
(let [icon-picker (require :icon-picker)]
  (icon-picker.setup {}))

;; }}}

;; inc-rename.nvim {{{
(let [inc-rename (require :inc_rename)]
  (inc-rename.setup))

;; }}}

;; remember.nvim {{{
(require :remember)
;; }}}
