(import-macros {: set! : set+ : g! : map!} :hibiscus.vim)

(when vim.env.PROF
  (let [snacks (or (and _G.USING_NIX (.. _G.LAZY_PLUGINS :/snacks-nvim))
                   (.. (vim.fn.stdpath :data) :/lazy/snacks.nvim))]
    (vim.opt.rtp:append snacks)
    (let [profiler (require :snacks.profiler)]
      (profiler.startup {}))))

(require :config.colors)

(g! :mapleader " ")
(g! :maplocalleader "\\")
(g! :editorconfig true)

(set! :breakindent true)
(set! :completeopt "menu,menuone,noinsert,noselect,preview")
(set! :conceallevel 2)
(set! :confirm true)
(set! :copyindent true)
(set! :cursorline true)
(set! :expandtab true)
(set! :foldmethod :marker)
(set! :guifont (.. _G.monoFont ":h12"))
(set! :hidden true)
(set! :ignorecase true)
(set! :inccommand :nosplit)
(set! :laststatus 3)
(set! :linebreak true)
(set! :mouse :a)
(set! :number true)
(set! :path "**")
(set! :preserveindent true)
(set! :redrawtime 10000)
(set! :relativenumber true)
(set! :scrolloff 2)
(set! :shiftround true)
(set! :shiftwidth 4)
(set! :showmode false)
(set! :sidescrolloff 8)
(set! :signcolumn "auto:3")
(set! :smartcase true)
(set! :smoothscroll true)
(set! :softtabstop 4)
(set! :splitbelow true)
(set! :splitkeep :screen)
(set! :splitright true)
(set! :tabstop 4)
(set! :termguicolors true)
(set! :timeoutlen 0)
(set! :undolevels 10000)
(set! :updatetime 200)
(set! :virtualedit :block)
(set! :winminwidth 5)
(set+ :clipboard :unnamedplus)
(set+ :shortmess :atcsqS)

(set! :fillchars {:fold " "
                  :foldopen "▾"
                  :foldclose "▸"
                  :eob " "
                  :diff "/"})

(map! [:nx :expr] :j #(if (= vim.v.count 0) :gj :j))
(map! [:nx :expr] :k #(if (= vim.v.count 0) :gk :k))

(map! [:n :remap] :<C-h> :<C-w>h)
(map! [:n :remap] :<C-j> :<C-w>j)
(map! [:n :remap] :<C-k> :<C-w>k)
(map! [:n :remap] :<C-l> :<C-w>l)

(map! [:n] :<C-A-h> "<cmd>resize +2<cr>")
(map! [:n] :<C-A-j> "<cmd>resize -2<cr>")
(map! [:n] :<C-A-k> "<cmd>vertical resize -2<cr>")
(map! [:n] :<C-A-l> "<cmd>vertical resize +2<cr>")

(map! [:n] :<A-j> :<cmd>bnext<cr>)
(map! [:n] :<A-k> :<cmd>bprevious<cr>)
(map! [:n] :<A-h> :<cmd>tabprevious<cr>)
(map! [:n] :<A-l> :<cmd>tabnext<cr>)

(map! [:ins] :<esc> :<cmd>noh<cr><esc>)
(map! [:v] "<" :<gv)
(map! [:v] ">" :>gv)

(map! [:n] :K #(vim.lsp.buf.hover {:border :solid}))

(set vim.deprecate (fn []))

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

(vim.diagnostic.config {:virtual_lines {:current_line true}
                        :severity_sort true})

; (let [lazy (require :lazy)
;       event (require :lazy.core.handler.event)]
;   (set event.mappings.LazyFile
;        {:id :LazyFile :event [:BufReadPost :BufNewFile :BufReadPre]})
;   (tset event :mappings "User LazyFile" event.mappings.LazyFile)
;   (lazy.setup [ (require :config.keybindings)
;                (require :config.languages)
;                (require :config.lsp)
;                (require :config.plugins.blink)
;                (require :config.plugins.heirline)
;                (require :config.plugins.mini)
;                (require :config.plugins.neo-tree)
;                (require :config.plugins.neorg)
;                (require :config.plugins.snacks)
;                (require :config.plugins.treesitter)
;                (require :config.ui)
;                (require :config.utilities)
;                (require :config.standalone)]
;               {:install {:missing (not _G.USING_NIX)}
;                :ui {:border :solid :backdrop 100}
;                :checker {:enabled (not _G.USING_NIX)}
;                :performance {:rtp {:reset (not _G.USING_NIX)}
;                              :reset_packpath false}
;                :readme {:enabled false}}))

(require :config.keybindings)
(require :config.languages)
(require :config.lsp)
(require :config.ui)
(require :config.utilities)
(require :config.plugins.blink)
(require :config.plugins.heirline)
(require :config.plugins.mini)
(require :config.plugins.neo-tree)
(require :config.plugins.neorg)
(require :config.plugins.snacks)
(require :config.plugins.treesitter)

(set! :loadplugins true)
