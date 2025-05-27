(import-macros {: use! : tx!} :config.macros)
(import-macros {: merge!} :hibiscus.core)
(import-macros {: augroup! : set!} :hibiscus.vim)

(macro pad [content]
  `(.. " " ,content " "))

(macro padl [content]
  `(.. " " ,content))

(macro padr [content]
  `(.. ,content " "))

(fn config []
  (set! :showtabline 2)
  (var buflist-cache [])
  (augroup! :heirline [[:VimEnter :UIEnter :BufAdd :BufDelete]
                       "*"
                       #(vim.schedule #(let [buffers (vim.tbl_filter #(vim.api.nvim_get_option_value :buflisted
                                                                                                     {:buf $1})
                                                                     (vim.api.nvim_list_bufs))]
                                         (each [i v (ipairs buffers)]
                                           (tset buflist-cache i v))
                                         (for [i (+ (length buffers) 1) (length buflist-cache)]
                                           (tset buflist-cache i nil))
                                         (if (> (length buflist-cache) 1)
                                             (set vim.o.showtabline 2)
                                             (not= vim.o.showtabline 1)
                                             (set vim.o.showtabline 1))))
                       [:FileType]
                       "*"])
  (let [heirline (require :heirline)
        conditions (require :heirline.conditions)
        utils (require :heirline.utils)
        icons (require :mini.icons)]
    (local components
           {; mode {{{
            :mode {:provider #(pad (. $1.mode-names $1.mode))
                   :update (tx! :ModeChanged
                                {:pattern "*:*"
                                 :callback #(vim.schedule_wrap (vim.cmd.redrawstatus))})
                   :hl #($1:mode-hl)}
            ; }}}
            ; filename {{{
            :filename [{:init #(do
                                 (set $1.bufnr (or $1.bufnr 0))
                                 (set $1.filename
                                      (vim.api.nvim_buf_get_name $1.bufnr)))
                        :provider #(pad (let [filename (vim.fn.fnamemodify $1.filename
                                                                           ":.")]
                                          (if (= filename "") "[No Name]"
                                              filename)))}
                       {:condition #(vim.api.nvim_get_option_value :modified
                                                                   {:buf $1.bufnr})
                        :provider (padr "󰆓")}
                       {:condition #(or (not (vim.api.nvim_get_option_value :modifiable
                                                                            {:buf $1.bufnr}))
                                        (vim.api.nvim_get_option_value :readonly
                                                                       {:buf $1.bufnr}))
                        :provider (padr "󰌾")}]
            ; }}}
            ; fileicon {{{
            :fileicon {:init #(do
                                (set $1.bufnr (or $1.bufnr 0))
                                (set $1.filename
                                     (vim.api.nvim_buf_get_name $1.bufnr))
                                (let [(icon hl _default) (icons.get :filetype
                                                                    (vim.api.nvim_get_option_value :filetype
                                                                                                   {:buf $1.bufnr}))]
                                  (set $1.icon icon)
                                  (set $1.hl hl)))
                       :provider #(and $1.icon (padl $1.icon))
                       :hl #$1.hl}
            ; }}}
            :filetype {:provider #(and vim.bo.filetype (padl vim.bo.filetype))}
            :fileformat {:provider #(padl (case vim.bo.fileformat
                                            :unix ""
                                            :dos ""))}
            ; lsp {{{
            :lsp {:condition conditions.lsp_attached
                  :update [:LspAttach :LspDetach]
                  :provider #(padr (.. "󰒓 "
                                       (table.concat (merge! (icollect [_ server (pairs (vim.lsp.get_clients {:bufnr 0}))]
                                                               server.name)
                                                             (if (. package.loaded
                                                                    :lint)
                                                                 (let [lint (require :lint)]
                                                                   (lint.get_running 0))))
                                                     " ")))
                  :hl {:fg _G.base03}}
            ; }}}
            ; diagnostics {{{
            :diagnostics (tx! [{:provider #(and (> $1.errors 0)
                                                (padr (.. $1.error-icon " "
                                                          $1.errors)))
                                :hl {:fg _G.base08}}
                               {:provider #(and (> $1.warnings 0)
                                                (padr (.. $1.warn-icon " "
                                                          $1.warnings)))
                                :hl {:fg _G.base0A}}
                               {:provider #(and (> $1.hints 0)
                                                (padr (.. $1.hint-icon " "
                                                          $1.hints)))
                                :hl {:fg _G.base0B}}
                               {:provider #(and (> $1.info 0)
                                                (padr (.. $1.info-icon " "
                                                          $1.info)))
                                :hl {:fg _G.base0D}}]
                              {:condition conditions.has_diagnostics
                               :static {:error-icon (. (vim.fn.sign_getdefined :DiagnosticSignError)
                                                       1 :text)
                                        :warn-icon (. (vim.fn.sign_getdefined :DiagnosticSignWarn)
                                                      1 :text)
                                        :hint-icon (. (vim.fn.sign_getdefined :DiagnosticSignHint)
                                                      1 :text)
                                        :info-icon (. (vim.fn.sign_getdefined :DiagnosticSignInfo)
                                                      1 :text)}
                               :init #(do
                                        (set $1.errors
                                             (length (vim.diagnostic.get 0
                                                                         {:severity vim.diagnostic.severity.ERROR})))
                                        (set $1.warnings
                                             (length (vim.diagnostic.get 0
                                                                         {:severity vim.diagnostic.severity.WARN})))
                                        (set $1.hints
                                             (length (vim.diagnostic.get 0
                                                                         {:severity vim.diagnostic.severity.HINT})))
                                        (set $1.info
                                             (length (vim.diagnostic.get 0
                                                                         {:severity vim.diagnostic.severity.INFO}))))
                               :update [:DiagnosticChanged :BufEnter]})
            ; }}}
            ; git {{{
            :git (tx! {:provider #(padr (.. " " $1.status.head))}
                      {:provider #(let [count (or $1.status.added 0)]
                                    (and (> count 0) (padr (.. " " count))))
                       :hl {:fg _G.base0B}}
                      {:provider #(let [count (or $1.status.changed 0)]
                                    (and (> count 0) (padr (.. " " count))))
                       :hl {:fg _G.base0A}}
                      {:provider #(let [count (or $1.status.removed 0)]
                                    (and (> count 0) (padr (.. " " count))))
                       :hl {:fg _G.base08}}
                      {:condition conditions.is_git_repo
                       :init #(do
                                (set $1.status vim.b.gitsigns_status_dict)
                                (set $1.has-changes
                                     (or (not= $1.status.added 0)
                                         (not= $1.status.removed 0)
                                         (not= $1.status.changed 0))))})
            ; }}}
            :ruler {:provider (pad "%l/%L:%c %P") :hl #($1:mode-hl)}
            :navic {:condition #(and (conditions.lsp_attached)
                                     (let [navic (require :nvim-navic)]
                                       (navic.is_available)))
                    :provider #(let [navic (require :nvim-navic)]
                                 (padl (navic.get_location)))
                    :update :CursorMoved}
            :spacer {:provider "%="}
            :indent {:init #(set $1.shiftwidth vim.bo.shiftwidth)
                     :provider #(padl (.. "󰌒 " $1.shiftwidth))}})
    (heirline.setup {:statusline (tx! (tx! components.mode components.fileicon
                                           components.filetype components.spacer
                                           {:condition #(conditions.buffer_matches {:buftype [:nofile]
                                                                                    :filetype [:TelescopePrompt
                                                                                               :DressingInput
                                                                                               :Trouble
                                                                                               :trouble
                                                                                               :lazy]})})
                                      (tx! components.mode
                                           {:provider #(padl (vim.fn.fnamemodify (vim.api.nvim_buf_get_name 0)
                                                                                 ":t"))}
                                           components.spacer
                                           {:condition #(conditions.buffer_matches {:buftype [:help]})})
                                      (tx! components.mode
                                           {:provider #(padl (let [(tname _) (: (vim.api.nvim_buf_get_name 0)
                                                                                :gsub
                                                                                ".*:"
                                                                                "")]
                                                               tname))}
                                           components.spacer
                                           {:condition #(conditions.buffer_matches {:buftype [:terminal]})})
                                      (tx! {:provider #(pad (vim.fn.fnamemodify (vim.fn.getcwd ":~")))
                                            :hl {:fg _G.base00
                                                 :bg _G.base0F
                                                 :bold true}}
                                           components.spacer
                                           {:condition #(conditions.buffer_matches {:filetype [:NvimTree
                                                                                               :neo-tree]})})
                                      [components.mode
                                       components.fileicon
                                       components.filetype
                                       components.indent
                                       components.fileformat
                                       components.spacer
                                       components.lsp
                                       components.git
                                       components.ruler]
                                      {:hl {:bg _G.base02 :fg _G.base05}
                                       :init #(set $1.mode (vim.fn.mode 1))
                                       :fallthrough false
                                       ; static {{{
                                       :static (let [n :NORMAL
                                                     n? :NORMAL?
                                                     v :VISUAL
                                                     vl :V-LINE
                                                     vb :V-BLOCK
                                                     s :SELECT
                                                     sl :S-LINE
                                                     sb :S-BLOCK
                                                     i :INSERT
                                                     r :REPLACE
                                                     c :COMMAND
                                                     ex :EX
                                                     p :PROMPT
                                                     ! :SHELL
                                                     t :TERMINAL]
                                                 {:mode-names {: n
                                                               :no n?
                                                               :nov n?
                                                               "no\022" n?
                                                               :niI n
                                                               :niR n
                                                               :niV n
                                                               :nt n
                                                               :ntT n
                                                               : v
                                                               :vs v
                                                               :V vl
                                                               :Vs vl
                                                               "\022" vb
                                                               "\022s" vb
                                                               : s
                                                               :S sl
                                                               "\019" sb
                                                               : i
                                                               :ic i
                                                               :ix i
                                                               :R r
                                                               :Rc r
                                                               :Rx r
                                                               :Rv r
                                                               :Rvc r
                                                               :Rvx r
                                                               : c
                                                               :cv ex
                                                               :r p
                                                               :rm p
                                                               :r? p
                                                               : !
                                                               : t}
                                                  :mode-colors {n _G.base0F
                                                                n? _G.base0F
                                                                v _G.base0A
                                                                vl _G.base0A
                                                                vb _G.base0A
                                                                s _G.base0D
                                                                sl _G.base0D
                                                                sb _G.base0D
                                                                i _G.base0B
                                                                r _G.base08
                                                                c _G.base0E
                                                                ex _G.base0E
                                                                p _G.base0F
                                                                ! _G.base0F
                                                                t _G.base0F}
                                                  :mode-hl #(let [mode (if (conditions.is_active)
                                                                           (vim.fn.mode)
                                                                           :n)]
                                                              {:fg _G.base00
                                                               :bg (. $1.mode-colors
                                                                      (. $1.mode-names
                                                                         mode))
                                                               :bold true})})})
                     ; }}}
                     :winbar (tx! (tx! components.fileicon components.filename
                                       {:condition #(not (conditions.is_active))
                                        :hl #{:fg (if vim.bo.modified
                                                      _G.base08
                                                      _G.base03)
                                              :bg _G.base00}})
                                  [(tx! components.fileicon components.filename
                                        {:hl #{:fg _G.base00
                                               :bg (if vim.bo.modified
                                                       _G.base08
                                                       _G.base0F)
                                               :bold true
                                               :force true}})
                                   components.navic
                                   components.spacer
                                   components.diagnostics]
                                  {:fallthrough false :hl {:bg _G.base00}})
                     :tabline (tx! (utils.make_buflist (tx! components.fileicon
                                                            components.filename
                                                            {:condition #(not $1.is_visible)
                                                             :on_click {:callback (fn [_
                                                                                       minwid
                                                                                       _
                                                                                       button]
                                                                                    (if (= button
                                                                                           :m)
                                                                                        (vim.schedule #(vim.api.nvim_buf_delete minwid
                                                                                                                                {:force false}))
                                                                                        (vim.api.nvim_win_set_buf 0
                                                                                                                  minwid)))
                                                                        :minwid #$1.bufnr
                                                                        :name :heirline_tabline_buffer_callback}
                                                             :hl {:bg _G.base02
                                                                  :fg _G.base03}})
                                                       {} {} #buflist-cache
                                                       false)
                                   {:provider "%="}
                                   (tx! (utils.make_tablist {:provider #(.. "%"
                                                                            $1.tabnr
                                                                            "T "
                                                                            $1.tabpage
                                                                            " %T")
                                                             :hl #(if $1.is_active
                                                                      {:fg _G.base00
                                                                       :bg _G.base0F
                                                                       :bold true}
                                                                      {:fg _G.base03
                                                                       :bg _G.base02})})
                                        {:condition #(>= (length (vim.api.nvim_list_tabpages))
                                                         2)})
                                   {:hl {:bg _G.base00}})
                     :opts {:disable_winbar_cb #(conditions.buffer_matches {:buftype [:nofile
                                                                                      :prompt
                                                                                      :terminal
                                                                                      :help]
                                                                            :filetype [:NvimTree
                                                                                       :Trouble
                                                                                       :WhichKey
                                                                                       :TelescopePrompt
                                                                                       :TelescopeResult
                                                                                       :Trouble
                                                                                       :lazy
                                                                                       :saga_codeaction
                                                                                       :neo-tree-preview
                                                                                       :snacks_picker_preview]}
                                                                           $1.buf)}})))

[(use! :rebelot/heirline.nvim {:event :VeryLazy : config})]
