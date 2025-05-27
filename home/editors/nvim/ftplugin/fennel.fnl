(import-macros {: setlocal!} :hibiscus.vim)
(import-macros {: map!} :hibiscus.vim)

(setlocal! :shiftwidth 2)

(map! [:n :buffer] :<localleader>lp :<cmd>FnlPeek<cr> "Peek Lua output")
