(require-macros :hibiscus.vim)

;; indent-blankline.nvim {{{
(let [indent_blankline (require :indent_blankline)]
  (indent_blankline.setup {:show_current_context true
                           :char "│"
                           :use_treesitter true
                           :filetype_exclude [:help :TelescopePrompt :neo-tree]
                           :buftype_exclude [:terminal]
                           :show_foldtext false
                           :indent_level 30}))

;; }}}

;; nvim-colorizer.lua {{{
(let [colorizer (require :colorizer)]
  (colorizer.setup {1 "*"
                    :DEFAULT_OPTIONS {:RRGGBBAA true :css true :css_fn true}}))

;; }}}

;; gitsigns.nvim {{{
(let [gitsigns (require :gitsigns)]
  (gitsigns.setup {:diff_opts {:internal true}}))

;; }}}

;; FTerm.nvim {{{
(let [fterm (require :FTerm)]
  (set _G.fterm_float (fterm:new {:hl :NormalFloat :border :solid})))

;; }}}

;; mini.nvim {{{
(let [trailspace (require :mini.trailspace)]
  (trailspace.setup))

;; }}}

;; fm-nvim {{{
(let [fm-nvim (require :fm-nvim)]
  (fm-nvim.setup {:ui {:float {:border :solid :float_hl :NormalPopover}}}))

;; nvim-hlslens {{{
(let [hlslens (require :hlslens)]
  (hlslens.setup))

;; }}}

;; pretty-fold.nvim {{{
(let [pretty-fold (require :pretty-fold)]
  (pretty-fold.setup {:fill_char " "
                      :process_comment_signs false
                      :sections {:left [:content]
                                 :right [:number_of_folded_lines]}}))

;; }}}

;; dressing.nvim {{{
(let [dressing (require :dressing)
      themes (require :telescope.themes)]
  (dressing.setup {:input {:border :solid :winblend 0}
                   :select {:backend [:telescope :nui :builtin]
                            :telescope (themes.get_cursor {:border true
                                                           :borderchars [" "
                                                                         " "
                                                                         " "
                                                                         " "
                                                                         " "
                                                                         " "
                                                                         " "
                                                                         " "]})}}))

;; }}}

;; cinnamon.nvim {{{
(let [cinnamon (require :cinnamon)]
  (cinnamon.setup {:extra_keymaps true :default_delay 3 :centered false}))

;; }}}

;; flare.nvim {{{
(let [flare (require :flare)]
  (flare.setup {:hl_group :PmenuSel}))

;; }}}

;; foldsigns.nvim {{{
(let [foldsigns (require :foldsigns)]
  (foldsigns.setup))

;; }}}

;; todo-comments.nvim {{{
(let [todo-comments (require :todo-comments)]
  (todo-comments.setup {}))

;; }}}

;; trouble.nvim {{{
(let [trouble (require :trouble)]
  (trouble.setup {:auto_preview false}))

;; }}}

;; hlargs.nvim {{{
(let [hlargs (require :hlargs)]
  (hlargs.setup))

;; }}}

;; hover.nvim {{{
(let [hover (require :hover)]
  (hover.setup {:init #(do
                         (require :hover.providers.lsp)
                         (require :hover.providers.gh)
                         (require :hover.providers.man))
                :preview_opts {:border :solid}})
  (map! [n] :K hover.hover)
  (map! [n] :gK hover.hover_select))

;; }}}

;; neo-tree.nvim {{{
(let [neo-tree (require :neo-tree)
      bufremove (require :mini.bufremove)]
  (neo-tree.setup {:sources [:buffers :filesystem]
                   :popup_border_style :solid
                   :use_default_mappings false
                   :default_component_configs {:container {:enable_character_fade false}
                                               :name {:trailing_slash true
                                                      :use_git_status_colors false}
                                               :git_status {:symbols {:added ""
                                                                      :deleted ""
                                                                      :modified ""
                                                                      :renamed ""
                                                                      :untracked ""
                                                                      :ignored ""
                                                                      :unstaged "ﱡ"
                                                                      :staged ""
                                                                      :conflict ""}
                                                            :align :left}
                                               :modified {:symbol "ﱣ"}}
                   :window {:width 30
                            :mappings {:<2-LeftMouse> :open
                                       :<cr> :open
                                       :s :open_split
                                       :v :open_vsplit
                                       :R :refresh
                                       :a {1 :add
                                           :config {:show_path :relative}}
                                       :d :delete
                                       :r :rename
                                       :y :copy_to_clipboard
                                       :x :cut_to_clipboard
                                       :p :paste_from_clipboard
                                       :q :close_window
                                       :? :show_help}}
                   :filesystem {:window {:mappings {:H :toggle_hidden
                                                    :/ :fuzzy_finder
                                                    :h :navigate_up
                                                    :l :set_root}}
                                :filtered_items {:hide_dotfiles false
                                                 :hide_gitignored false}}
                   :buffers {:window {:mappings {:d :bdelete :D :bforce}}
                             :commands {:bdelete #(let [node ($1.tree:get_node)
                                                        path (node:get_id)]
                                                    (bufremove.delete node.extra.bufnr))
                                        :bforce #(let [node ($1.tree:get_node)
                                                       path (node:get_id)]
                                                   (bufremove.delete node.extra.bufnr
                                                                     true))}}}))

;; nvim-notify {{{
(let [notify (require :notify)
      spinner-frames ["⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷"]]
  (notify.setup {:fps 60})
  (set vim.notify notify)
  ;; copied and compiled to fennel using antifennel
  ;; https://github.com/rcarriga/nvim-notify/wiki/Usage-Recipes#progress-updates
  (local client-notifs {})

  (fn get-notif-data [client-id token]
    (when (not (. client-notifs client-id))
      (tset client-notifs client-id {}))
    (when (not (. (. client-notifs client-id) token))
      (tset (. client-notifs client-id) token {}))
    (. (. client-notifs client-id) token))

  (local spinner-frames ["⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷"])

  (fn update-spinner [client-id token]
    (let [notif-data (get-notif-data client-id token)]
      (when notif-data.spinner
        (local new-spinner (% (+ notif-data.spinner 1) (length spinner-frames)))
        (set notif-data.spinner new-spinner)
        (set notif-data.notification
             (vim.notify nil nil
                         {:replace notif-data.notification
                          :hide_from_history true
                          :icon (. spinner-frames new-spinner)}))
        (vim.defer_fn (fn []
                        (update-spinner client-id token))
                      100))))

  (fn format-title [title client-name]
    (.. client-name (or (and (> (length title) 0) (.. ": " title)) "")))

  (fn format-message [message percentage]
    (.. (or (and percentage (.. percentage "%\t")) "") (or message "")))

  ;; https://github.com/rcarriga/nvim-notify/wiki/Usage-Recipes#lsp-status-updates
  (tset vim.lsp.handlers :$/progress
        (fn [_ result ctx]
          (let [client-id ctx.client_id
                val result.value]
            (when (not val.kind)
              (lua "return "))
            (local notif-data (get-notif-data client-id result.token))
            (if (= val.kind :begin)
                (let [message (format-message val.message val.percentage)]
                  (set notif-data.notification
                       (vim.notify message :info
                                   {:timeout false
                                    :title (format-title val.title
                                                         (. (vim.lsp.get_client_by_id client-id)
                                                            :name))
                                    :hide_from_history false
                                    :icon (. spinner-frames 1)}))
                  (set notif-data.spinner 1)
                  (update-spinner client-id result.token))
                (and (= val.kind :report) notif-data)
                (set notif-data.notification
                     (vim.notify (format-message val.message val.percentage)
                                 :info
                                 {:replace notif-data.notification
                                  :hide_from_history false}))
                (and (= val.kind :end) notif-data)
                (do
                  (set notif-data.notification
                       (vim.notify (or (and val.message
                                            (format-message val.message))
                                       :Complete)
                                   :info
                                   {:timeout 3000
                                    :replace notif-data.notification
                                    :icon ""}))
                  (set notif-data.spinner nil))))))
  ;; https://github.com/rcarriga/nvim-notify/wiki/Usage-Recipes#lsp-messages
  (local severity [:error :warn :info :info])
  (tset vim.lsp.handlers :window/showMessage
        (fn [err method params client-id]
          (vim.notify method.message (. severity params.type)))))

;; }}}

;; neodim {{{
(let [neodim (require :neodim)]
  (neodim.setup {:blend_color _G.base00}))

;; }}}
