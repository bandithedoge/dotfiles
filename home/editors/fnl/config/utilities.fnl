;; mini.nvim {{{
(let [pairs (require :mini.pairs)
      surround (require :mini.surround)
      bufremove (require :mini.bufremove)]
  (pairs.setup {})
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
