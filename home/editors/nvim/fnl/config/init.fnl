(import-macros {: set! : set+ : g! : map!} :hibiscus.vim)

(when vim.env.PROF
  (let [snacks (or (and _G.USING_NIX (.. _G.LAZY_PLUGINS :/snacks-nvim))
                   (.. (vim.fn.stdpath :data) :/lazy/snacks.nvim))]
    (vim.opt.rtp:append snacks)
    (let [profiler (require :snacks.profiler)]
      (profiler.startup {}))))

(require :config.colors)

(set! :breakindent true)
(set! :completeopt "menu,menuone,noinsert,noselect,preview")
(set! :conceallevel 2)
(set! :copyindent true)
(set! :cursorline true)
(set! :expandtab true)
(set! :fillchars "fold: ,foldopen:▾,foldclose:▸,eob: ")
(set! :foldmethod :marker)
(set! :guifont (.. _G.monoFont ":h12"))
(set! :hidden true)
(set! :inccommand :split)
(set! :laststatus 3)
(set! :linebreak true)
(set! :mouse :a)
(set! :number true)
(set! :path "**")
(set! :preserveindent true)
(set! :redrawtime 10000)
(set! :relativenumber true)
(set! :scrolloff 2)
(set! :shiftwidth 4)
(set! :showmode false)
(set! :signcolumn "auto:3")
(set! :softtabstop 4)
(set! :splitbelow true)
(set! :splitkeep :screen)
(set! :splitkeep :screen)
(set! :splitright true)
(set! :tabstop 4)
(set! :termguicolors true)
(set! :timeoutlen 0)
(set! :updatetime 200)
(set+ :clipboard :unnamedplus)
(set+ :shortmess :atcsqS)

(g! :mapleader " ")
(g! :maplocalleader "\\")
(g! :editorconfig true)

(map! [:nx] :j :gj)
(map! [:nx] :k :gk)
(map! [:in] :<esc> :<cmd>noh<cr><esc>)
(map! [:n] :<A-j> :<cmd>bnext<cr>)
(map! [:n] :<A-k> :<cmd>bprevious<cr>)
(map! [:n] :<A-h> :<cmd>tabprevious<cr>)
(map! [:n] :<A-l> :<cmd>tabnext<cr>)

(when vim.g.neovide
  (let [padding 10]
    (g! :neovide_padding_top padding)
    (g! :neovide_padding_bottom padding)
    (g! :neovide_padding_right padding)
    (g! :neovide_padding_left padding))
  (g! :neovide_hide_mouse_when_typing true))

(vim.fn.sign_define :DiagnosticSignError
                    {:text "" :texthl :DiagnosticSignError})

(vim.fn.sign_define :DiagnosticSignWarn
                    {:text "" :texthl :DiagnosticSignWarn})

(vim.fn.sign_define :DiagnosticSignInfo
                    {:text "" :texthl :DiagnosticSignInfo})

(vim.fn.sign_define :DiagnosticSignHint
                    {:text "󰌵" :texthl :DiagnosticSignHint})

(let [lazy (require :lazy)
      event (require :lazy.core.handler.event)]
  (set event.mappings.LazyFile
       {:id :LazyFile :event [:BufReadPost :BufNewFile :BufReadPre]})
  (tset event :mappings "User LazyFile" event.mappings.LazyFile)
  (lazy.setup [(require :config.keybindings)
               (require :config.languages)
               (require :config.lsp)
               (require :config.plugins.blink)
               (require :config.plugins.heirline)
               (require :config.plugins.mini)
               (require :config.plugins.neo-tree)
               (require :config.plugins.neorg)
               (require :config.plugins.snacks)
               (require :config.plugins.telescope)
               (require :config.plugins.treesitter)
               (require :config.ui)
               (require :config.utilities)
               (require :config.standalone)]
              {:install {:missing (not _G.USING_NIX)}
               :checker {:enabled (not _G.USING_NIX)}
               :performance {:rtp {:reset (not _G.USING_NIX)}
                             :reset_packpath false}
               :readme {:enabled false}}))

(set! :loadplugins true)
