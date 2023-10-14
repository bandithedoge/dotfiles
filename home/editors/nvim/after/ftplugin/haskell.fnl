(require-macros :hibiscus.vim)

(let [haskell-tools (require :haskell-tools)]
  (map! [n :buffer] :<localleader>lh `haskell-tools.hoogle.hoogle_signature
        :Hoogle)
  (map! [n :buffer] :<localleader>lrr `haskell-tools.repl.toggle "GHCi repl")
  (map! [n :buffer] :<localleader>lrb
        #(haskell-tools.repl.toggle (vim.api.nvim_buf_get_name 0))
        "GHCi repl (buffer)")
  (map! [n :buffer] :<localleader>lrq `haskell-tools.repl.quit))
