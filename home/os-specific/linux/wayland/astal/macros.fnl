(local M {})

; https://github.com/bakpakin/Fennel/issues/353#issuecomment-2604946507
(fn M.tx! [& args]
  (let [to-merge (when (table? (. args (length args)))
                   (table.remove args))]
    (if to-merge
        (do
          (each [key value (pairs to-merge)]
            (tset args key value))
          args)
        args)))

M
