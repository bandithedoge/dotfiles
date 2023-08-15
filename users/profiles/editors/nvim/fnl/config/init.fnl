(require-macros :hibiscus.core)
(require-macros :hibiscus.vim)

(require :config.colors)

(set! breakindent true)
(set! completeopt "menu,menuone,noinsert,noselect,preview")
(set! conceallevel 2)
(set! copyindent true)
(set! cursorline true)
(set! expandtab true)
(set! fillchars "fold: ,foldopen:▾,foldclose:▸,eob: ")
(set! foldmethod :marker)
(set! guifont (.. _G.monoFont ::h12))
(set! hidden true)
(set! inccommand :split)
(set! linebreak true)
(set! mouse :a)
(set! number true)
(set! path "**")
(set! preserveindent true)
(set! redrawtime 10000)
(set! relativenumber true)
(set! scrolloff 2)
(set! shiftwidth 4)
(set! showmode false)
(set! signcolumn "auto:3")
(set! softtabstop 4)
(set! splitbelow true)
(set! splitkeep :screen)
(set! splitright true)
(set! tabstop 4)
(set! termguicolors true)
(set! timeoutlen 0)
(set! updatetime 200)
(set+ clipboard :unnamedplus)
(set+ shortmess :atcsqS)

(g! mapleader " ")
(g! maplocalleader "\\")

(map! [n] :<cr> ":noh<cr>")
(map! [n] :K vim.lsp.buf.hover)

(when vim.g.neovide
  (let [padding 10]
    (g! neovide_padding_top padding)
    (g! neovide_padding_bottom padding)
    (g! neovide_padding_right padding)
    (g! neovide_padding_left padding))
  (g! neovide_hide_mouse_when_typing true))

(lambda _G.use [?name ?opts ?alt-name]
  (if _G.USING_NIX
      (merge! {:dir (.. _G.LAZY_PLUGINS "/"
                        (string.gsub (string.match (or ?alt-name ?name) "/(.+)")
                                     "%." "-"))
               :name (string.match (or ?alt-name ?name) "/(.+)")}
              (or ?opts {}))
      (merge! [?name] (or ?opts {}))))

(lambda _G.key [lhs ?rhs ?opts]
  (merge! [lhs ?rhs] (or ?opts {})))

(let [lazy (require :lazy)]
  (lazy.setup [(require :config.dap)
               (require :config.keybindings)
               (require :config.languages)
               (require :config.lsp)
               (require :config.ui)
               (require :config.utilities)
               (require :config.plugins.cmp)
               (require :config.plugins.lualine)
               (require :config.plugins.luasnip)
               (require :config.plugins.mini)
               (require :config.plugins.neo-tree)
               (require :config.plugins.neorg)
               (require :config.plugins.telescope)
               (require :config.plugins.treesitter)
               (require :config.standalone)]
              {:install {:missing (not _G.USING_NIX)}
               :performance {:rtp {:reset (not _G.USING_NIX)}
                             :reset_packpath false}}))

(set! loadplugins true)
