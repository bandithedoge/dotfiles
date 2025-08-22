(import-macros {: load! : key!} :config.macros)
(import-macros {: g!} :hibiscus.vim)

(g! :parinfer_force_balance true)

(load! :lazydev.nvim {:ft :lua
                      :before #(let [lazydev (require :lazydev)]
                                 (lazydev.setup))})

(load! :typst-preview.nvim
       (let [ft :typst]
         {: ft
          :keys [(key! :<localleader>lp :<cmd>TypstPreview<cr>
                       {:desc :Preview : ft})]
          :after #(let [typst-preview (require :typst-preview)]
                    (typst-preview.setup))}))
