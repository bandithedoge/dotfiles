(require-macros :hibiscus.vim)
(set! shiftwidth 2)

(let [crates (require :crates)]
  (crates.setup {:null_ls {:enabled true}}))
