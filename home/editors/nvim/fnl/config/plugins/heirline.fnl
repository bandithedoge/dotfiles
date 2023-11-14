(require-macros :hibiscus.core)
(require-macros :hibiscus.vim)

(lambda pad [content]
  (.. " " content " "))

(lambda padl [content]
  (.. " " content))

(lambda padr [content]
  (.. content " "))

(fn config []
  (let [heirline (require :heirline)
        conditions (require :heirline.conditions)
        utils (require :heirline.utils)
        icons (require :nvim-web-devicons)]
    (local components
           {; mode {{{
            :mode (let [n :NORMAL
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
                    {:init #(set $1.mode (vim.fn.mode 1))
                     ; static {{{
                     :static {:mode-names {: n
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
                              :colors {n _G.base0F
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
                                       t _G.base0F}}
                     ; }}}
                     :provider #(pad (. $1.mode-names $1.mode))
                     :hl #{:fg _G.base00
                           :bg (. $1.colors (. $1.mode-names $1.mode))
                           :bold true}
                     :update (merge! [:ModeChanged]
                                     {:pattern "*:*"
                                      :callback #(vim.schedule_wrap (vim.cmd.redrawstatus))})})
            ; }}}
            ; filename {{{
            :filename [{:init #(set $1.filename (vim.api.nvim_buf_get_name 0))
                        :provider #(pad (let [filename (vim.fn.fnamemodify $1.filename
                                                                           ":.")]
                                          (if (= filename "") "[No Name]"
                                              filename)))}
                       {:condition #vim.bo.modified :provider (padr "󰆓")}
                       {:condition #(or (not vim.bo.modifiable) vim.bo.readonly)
                        :provider (padr "󰌾")}]
            ; }}}
            ; fileicon {{{
            :fileicon {:init #(do
                                (set $1.filename (vim.api.nvim_buf_get_name 0))
                                (let [(icon color) (icons.get_icon_color $1.filename
                                                                         (vim.fn.fnamemodify $1.filename
                                                                                             ":e")
                                                                         {:default true})]
                                  (set $1.icon icon)
                                  (set $1.color color)))
                       :provider #(and $1.icon (padl $1.icon))
                       :hl #{:fg $1.color}}
            ; }}}
            :filetype {:provider #(padl vim.bo.filetype)}
            :fileformat {:provider #(padl (case vim.bo.fileformat
                                            :unix ""
                                            :dos ""))}
            ; lsp {{{
            :lsp {:condition conditions.lsp_attached
                  :update [:LspAttach :LspDetach]
                  :provider #(padr (.. "󰒓 "
                                       (table.concat (icollect [_ server (pairs (vim.lsp.get_active_clients {:bufnr 0}))]
                                                       server.name)
                                                     " ")))
                  :hl {:fg _G.base03}}
            ; }}}
            ; diagnostics {{{
            :diagnostics (merge! {:condition conditions.has_diagnostics
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
                                  :update [:DiagnosticChanged :BufEnter]}
                                 [{:provider #(and (> $1.errors 0)
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
                                   :hl {:fg _G.base0D}}])
            ; }}}
            ; git {{{
            :git (merge! {:condition conditions.is_git_repo
                          :init #(do
                                   (set $1.status vim.b.gitsigns_status_dict)
                                   (set $1.has-changes
                                        (or (not= $1.status.added 0)
                                            (not= $1.status.removed 0)
                                            (not= $1.status.changed 0))))}
                         [{:provider #(padr (.. " " $1.status.head))}
                          {:provider #(let [count (or $1.status.added 0)]
                                        (and (> count 0)
                                             (padr (.. " " count))))
                           :hl {:fg _G.base0B}}
                          {:provider #(let [count (or $1.status.changed 0)]
                                        (and (> count 0)
                                             (padr (.. " " count))))
                           :hl {:fg _G.base0A}}
                          {:provider #(let [count (or $1.status.removed 0)]
                                        (and (> count 0)
                                             (padr (.. " " count))))
                           :hl {:fg _G.base08}}])
            ; }}}
            :navic {:condition conditions.lsp_attached
                    :init #(set $1.bar
                                (let [winbar (require :lspsaga.symbol.winbar)]
                                  (winbar.get_bar)))
                    :provider #(and $1.bar (padl $1.bar))}
            :spacer {:provider "%="}
            :indent {:init #(set $1.shiftwidth vim.bo.shiftwidth)
                     :provider #(padl (.. "󰌒 " $1.shiftwidth))}})
    (heirline.setup {:statusline (merge! {:hl {:bg _G.base02 :fg _G.base05}
                                          :fallthrough false}
                                         [(merge! {:condition #(conditions.buffer_matches {:buftype []
                                                                                           :filetype [:TelescopePrompt
                                                                                                      :DressingInput
                                                                                                      :Trouble
                                                                                                      :lazy
                                                                                                      :saga_codeaction]})}
                                                  [components.mode
                                                   components.spacer])
                                          (merge! {:condition #(conditions.buffer_matches {:buftype [:help]})}
                                                  [components.mode
                                                   {:provider #(padl (vim.fn.fnamemodify (vim.api.nvim_buf_get_name 0)
                                                                                         ":t"))}
                                                   components.spacer])
                                          (merge! {:condition #(conditions.buffer_matches {:buftype [:terminal]})}
                                                  [components.mode
                                                   {:provider #(padl (let [(tname _) (: (vim.api.nvim_buf_get_name 0)
                                                                                        :gsub
                                                                                        ".*:"
                                                                                        "")]
                                                                       tname))}
                                                   components.spacer])
                                          (merge! {:condition #(conditions.buffer_matches {:filetype [:NvimTree]})}
                                                  [{:provider #(pad (vim.fn.fnamemodify (vim.fn.getcwd)
                                                                                        ":~"))
                                                    :hl {:fg _G.base00
                                                         :bg _G.base0F
                                                         :bold true}}
                                                   components.spacer])
                                          [components.mode
                                           components.fileicon
                                           components.filetype
                                           components.indent
                                           components.fileformat
                                           components.spacer
                                           components.diagnostics
                                           components.lsp
                                           components.git]])
                     :winbar (merge! {:fallthrough false :hl {:bg _G.base00}}
                                     [(merge! {:condition #(not (conditions.is_active))
                                               :hl #{:fg (if vim.bo.modified
                                                             _G.base08
                                                             _G.base03)
                                                     :bg _G.base00}}
                                              [components.fileicon
                                               components.filename])
                                      [(merge! {:hl #{:fg _G.base00
                                                      :bg (if vim.bo.modified
                                                              _G.base08
                                                              _G.base0F)
                                                      :bold true
                                                      :force true}}
                                               [components.fileicon
                                                components.filename])
                                       components.navic
                                       components.spacer
                                       components.diagnostics]])
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
                                                                                       :saga_codeaction]}
                                                                           $1.buf)}})
    (set! showtabline 2)))

[(_G.use :rebelot/heirline.nvim {:dependencies [(_G.use :nvim-tree/nvim-web-devicons)]
                                 : config})]
