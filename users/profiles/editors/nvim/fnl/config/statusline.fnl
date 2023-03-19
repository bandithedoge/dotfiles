(require-macros :hibiscus.core)

(set vim.o.laststatus 3)

(let [heirline (require :heirline)
      conditions (require :heirline.conditions)
      utils (require :heirline.utils)
      file-icon {:init #(let [nvim-web-devicons (require :nvim-web-devicons)
                              filename $1.filename]
                          (set ($1.icon $1.icon_color)
                               (nvim-web-devicons.get_icon_color filename
                                                                 (vim.fn.fnamemodify filename
                                                                                     ":e")
                                                                 {:default true})))
                 :provider #(and $1.icon (.. " " $1.icon " "))
                 :hl #{:fg $1.icon_color}}]
  (heirline.load_colors {:bright_bg _G.base02
                         :bright_fg _G.base05
                         :red _G.base08
                         :dark_red _G.base08
                         :green _G.base0B
                         :blue _G.base0D
                         :gray _G.base03
                         :orange _G.base09
                         :purple _G.base0E
                         :cyan _G.base0C
                         :diag_warn _G.base09
                         :diag_error _G.base08
                         :diag_hint _G.base0B
                         :diag_info _G.base0D
                         :git_del _G.base08
                         :git_add _G.base0B
                         :git_change _G.base09})
  (heirline.setup (let [align {:provider "%="}]
                    (merge {:hl {:bg _G.base01}}
                           [;; mode {{{
                            {:init (fn [self]
                                     (set self.mode (vim.fn.mode 1))
                                     (when (not self.once)
                                       (do
                                         (vim.api.nvim_create_autocmd :ModeChanged
                                                                      {:pattern "*:*o"
                                                                       :command :redrawstatus})
                                         (set self.once true))))
                             :static {:mode_names (let [n :NORMAL
                                                        v :VISUAL
                                                        s :SELECT
                                                        i :INSERT
                                                        r :REPLACE
                                                        c :COMMAND
                                                        q "?"
                                                        t :TERMINAL]
                                                    {: n
                                                     :no n
                                                     :nov n
                                                     :noV n
                                                     "no\022" n
                                                     :niI n
                                                     :niR n
                                                     :niV n
                                                     :nt n
                                                     : v
                                                     :vs v
                                                     :V v
                                                     :Vs v
                                                     "\022" v
                                                     "\022s" v
                                                     : s
                                                     :S s
                                                     "\019" s
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
                                                     :cv c
                                                     :r q
                                                     :rm q
                                                     :r? q
                                                     :! t
                                                     : t})
                                      :mode_colors {:n _G.base0F
                                                    :i _G.base0B
                                                    :v _G.base0A
                                                    :V _G.base0A
                                                    "\022" _G.base0A
                                                    :c _G.base0E
                                                    :s _G.base0D
                                                    :S _G.base0D
                                                    "\019" _G.base0D
                                                    :R _G.base08
                                                    :r _G.base08
                                                    :! _G.base0C
                                                    :t _G.base0C}}
                             :provider #(.. "%2( " (. $1.mode_names $1.mode)
                                            " %)")
                             :hl #{:bg (. $1.mode_colors ($1.mode:sub 1 1))
                                   :fg _G.base00
                                   :bold true}
                             :update [:ModeChanged :BufEnter]}
                            ;; }}}
                            ;; filename {{{
                            (merge {:init #(set $1.filename
                                                (vim.api.nvim_buf_get_name 0))
                                    :hl {:bg _G.base02}}
                                   [file-icon
                                    (merge {:hl #(when vim.bo.modified
                                                   {:fg _G.base08
                                                    :bold true
                                                    :force true})}
                                           [{:provider #(do
                                                          (var filename
                                                               (vim.fn.fnamemodify $1.filename
                                                                                   ":."))
                                                          (if (= filename "")
                                                              "[No Name]"
                                                              (not (conditions.width_percent_below (length filename)
                                                                                                   0.25))
                                                              (set filename
                                                                   (vim.fn.pathshorten filename)))
                                                          filename)
                                             :hl {:fg _G.base05}}])
                                    (unpack [{:condition #vim.bo.modified
                                              :provider " "
                                              :hl {:fg _G.base0B}}
                                             {:condition #(or (not vim.bo.modifiable)
                                                              vim.bo.readonly)
                                              :provider " "
                                              :hl {:fg _G.base08}}])
                                    {:provider "%< "}])
                            ;; }}}
                            ;; lsp and diagnostics {{{
                            (merge {:hl {:bg _G.base01}}
                                   [{:condition conditions.lsp_attached
                                     :update [:LspAttach :LspDetach :BufEnter]
                                     :provider (fn []
                                                 (local servers [])
                                                 (vim.lsp.for_each_buffer_client 0
                                                                                 #(table.insert servers
                                                                                                $1.name))
                                                 (.. " "
                                                     (table.concat servers " ")
                                                     " "))
                                     :hl {:fg _G.base03}}
                                    (merge {:condition conditions.has_diagnostics
                                            :static {:error_icon " "
                                                     :warn_icon " "
                                                     :info_icon " "
                                                     :hint_icon " "}
                                            :update [:DiagnosticChanged
                                                     :BufEnter]
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
                                                                                      {:severity vim.diagnostic.severity.INFO}))))}
                                           [{:provider #(and (> $1.errors 0)
                                                             (.. $1.error_icon
                                                                 $1.errors " "))
                                             :hl {:fg _G.base08}}
                                            {:provider #(and (> $1.warnings 0)
                                                             (.. $1.warn_icon
                                                                 $1.warnings " "))
                                             :hl {:fg _G.base0A}}
                                            {:provider #(and (> $1.info 0)
                                                             (.. $1.info_icon
                                                                 $1.info " "))
                                             :hl {:fg _G.base0D}}
                                            {:provider #(and (> $1.hints 0)
                                                             (.. $1.hint_icon
                                                                 $1.hints " "))
                                             :hl {:fg _G.base0B}}])])
                            ;; }}}
                            align
                            ;; git {{{
                            (merge {:condition conditions.is_git_repo
                                    :init #(do
                                             (set $1.status_dict
                                                  vim.b.gitsigns_status_dict)
                                             (set $1.has_changes
                                                  (or (not= $1.status_dict.added
                                                            0)
                                                      (not= $1.status_dict.removed
                                                            0)
                                                      (not= $1.status_dict.changed
                                                            0))))
                                    :hl {:bg _G.base01}}
                                   [{:provider #(.. "  " $1.status_dict.head
                                                    " ")
                                     :hl {:fg _G.base03}}
                                    (merge {:condition #$1.has_changes}
                                           [{:provider #(let [count (or $1.status_dict.added
                                                                        0)]
                                                          (and (> count 0)
                                                               (.. " " count
                                                                   " ")))
                                             :hl {:fg _G.base0B}}
                                            {:provider #(let [count (or $1.status_dict.removed
                                                                        0)]
                                                          (and (> count 0)
                                                               (.. " " count
                                                                   " ")))
                                             :hl {:fg _G.base08}}
                                            {:provider #(let [count (or $1.status_dict.changed
                                                                        0)]
                                                          (and (> count 0)
                                                               (.. " " count
                                                                   " ")))
                                             :hl {:fg _G.base0A}}])])
                            ;; }}}
                            ;; file format {{{
                            (merge {:hl {:bg _G.base02}}
                                   [{:provider #(.. " " vim.bo.filetype)
                                     :hl {:fg _G.base03}}
                                    {:provider #(.. " "
                                                    (match vim.bo.fileformat
                                                      :dos ""
                                                      :unix ""
                                                      :mac ""))}
                                    {:provider #(.. " " vim.bo.fenc " ")}
                                    {:provider "%l:%c " :hl {:fg _G.base03}}])]))
                  ;; }}}
                  [{:init #(set vim.opt_local.winbar nil)}]
                  (utils.make_buflist [(merge {:init #(set $1.filename
                                                           (vim.api.nvim_buf_get_name $1.bufnr))
                                               :hl #(if $1.is_active
                                                        {:fg _G.base00
                                                         :bg _G.base0F
                                                         :force true}
                                                        {:fg _G.base04
                                                         :bg _G.base02})
                                               :on_click {:callback #(vim.api.nvim_win_set_buf 0
                                                                                               $2)
                                                          :minwid #$1.bufnr
                                                          :name :heirline_tabline_buffer_callback}}
                                              [file-icon
                                               {:provider #(let [filename $1.filename]
                                                             (set $1.filename
                                                                  (or (and (= filename
                                                                              "")
                                                                           "[No Name]")
                                                                      (vim.fn.fnamemodify filename
                                                                                          ":t")))
                                                             (.. filename " "))}
                                               [{:condition #(vim.api.nvim_buf_get_option $1.bufnr
                                                                                          :modified)
                                                 :provider " "
                                                 :hl {:fg _G.base0}}
                                                {:condition #(or (not (vim.api.nvim_buf_get_option $1.bufnr
                                                                                                   :modifiable))
                                                                 (vim.api.nvim_buf_get_option $1.bufnr
                                                                                              :readonly))
                                                 :provider " "
                                                 :hl {:fg _G.base08}}]])])))

(vim.cmd "au FileType * if index(['wipe', 'delete'], &bufhidden) >= 0 | set nobuflisted | endif")
(vim.cmd "au FileType * setlocal winbar=")
