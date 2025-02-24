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

(fn M.use! [name opts alt-name]
  (let [opts# (or opts {})
        name# (string.match (or alt-name name) "/(.+)")]
    (if _G.USING_NIX
        (do
          (tset opts# :dir
                (.. _G.LAZY_PLUGINS "/" (string.gsub name# "%." "-")))
          (tset opts# :name name#))
        (tset opts# 1 name#))
    opts#))

(fn M.key! [lhs rhs opts]
  (let [opts# (or opts {})]
    (tset opts# 1 lhs)
    (tset opts# 2 rhs)
    opts#))

M
