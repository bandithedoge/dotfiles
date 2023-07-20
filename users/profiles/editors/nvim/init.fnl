(require-macros :hibiscus.core)
(require-macros :hibiscus.vim)

(require :config.colors)

(set vim.g.mapleader " ")
(set vim.o.breakindent true)
(set vim.o.clipboard (.. vim.o.clipboard :unnamedplus))
(set vim.o.completeopt "menu,menuone,noinsert,noselect,preview")
(set vim.o.conceallevel 2)
(set vim.o.fillchars "fold: ,foldopen:▾,foldclose:▸,eob: ")
(set vim.o.hidden true)
(set vim.o.inccommand :split)
(set vim.o.linebreak true)
(set vim.o.mouse :a)
(set vim.o.number true)
(set vim.o.path "**")
(set vim.o.redrawtime 10000)
(set vim.o.relativenumber true)
(set vim.o.scrolloff 2)
(set vim.o.showmode false)
(set vim.o.signcolumn "auto:3")
(set vim.o.splitbelow true)
(set vim.o.splitright true)
(set vim.o.termguicolors true)
(set vim.o.timeoutlen 0)
(set vim.o.updatetime 200)
(set vim.wo.cursorline true)
(set vim.wo.foldmethod :marker)
(vim.opt.shortmess:append :atcsqS)
(set vim.o.splitkeep :screen)

(set vim.o.copyindent true)
(set vim.o.expandtab true)
(set vim.o.preserveindent true)
(set vim.o.shiftwidth 4)
(set vim.o.softtabstop 4)
(set vim.o.tabstop 4)

(g! mapleader " ")
(g! maplocalleader "\\")

(map! [n] :<cr> ":noh<cr>")

(vim.cmd "au BufReadPre *.nfo :setlocal fileencodings=cp437,utf-8")

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
  (lazy.setup [(require :config.completion)
               (require :config.dap)
               (require :config.keybindings)
               (require :config.languages)
               (require :config.lsp)
               (require :config.snippets)
               (require :config.statusline)
               (require :config.telescope)
               (require :config.treesitter)
               (require :config.ui)
               (require :config.utilities)
               (require :config.writing)]
              {:install {:missing (not _G.USING_NIX)}}))

(set vim.o.loadplugins true)
