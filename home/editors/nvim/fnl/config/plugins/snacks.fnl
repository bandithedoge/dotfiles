(import-macros {: use! : key! : tx!} :config.macros)

[(use! :folke/snacks.nvim
       {:lazy false
        :priority 1000
        :keys [(key! :<leader>G #(_G.Snacks.gitbrowse)
                     {:desc "Open repo in browser"})
               (key! :<leader>T #(_G.Snacks.terminal.toggle) {:desc :Terminal})
               (key! :<leader>g #(_G.Snacks.lazygit.open) {:desc :Git})
               (key! :<leader><space>
                     #(_G.Snacks.picker.commands {:layout :ivy})
                     {:desc "Enter command"})
               (key! :<leader>b #(_G.Snacks.picker.buffers {:layout :ivy})
                     {:desc :Buffers})
               (key! :<leader>i #(_G.Snacks.picker.icons {:layout :select})
                     {:desc "Insert symbol"})
               (key! :<leader>n #(_G.Snacks.picker.notifications)
                     {:desc "Notification history"})
               (key! :<leader>ff #(_G.Snacks.picker.files) {:desc :Files})
               (key! :<leader>fF #(_G.Snacks.picker.recent)
                     {:desc "Recent files"})
               (key! :<leader>fg #(_G.Snacks.picker.grep) {:desc :Files})
               (key! :<leader>fh #(_G.Snacks.picker.help) {:desc :Help})
               (key! :<leader>fH #(_G.Snacks.picker.highlights)
                     {:desc "Highlight groups"})
               (key! :<leader>fp #(_G.Snacks.picker) {:desc :Pickers})
               (key! :<leader>fk #(_G.Snacks.picker.keymaps) {:desc :Keymaps})
               (key! :<leader>fm #(_G.Snacks.picker.man) {:desc :Man})
               (key! :<leader>fu #(_G.Snacks.picker.undo) {:desc :Undo})]
        :opts {:styles {:dashboard {:wo {:winhighlight "CursorLine:Normal,Normal:NormalNC,MiniTrailspace:NormalNC"}}
                        :input {:border :solid
                                :relative :cursor
                                :row -3
                                :col 0}
                        :notification {:border :solid :wo {:winblend 0}}
                        :float {:backdrop 100}}
               :bigfile {:enabled true}
               :dashboard {:enabled true
                           :sections [{:section :keys :gap 1 :padding 1}
                                      {:section :recent_files :padding 1}
                                      {:section :startup :icon " "}]
                           :preset {:keys [{:icon "󰈔"
                                            :key :n
                                            :desc "New file"
                                            :action ":ene"}
                                           {:icon "󰉋"
                                            :key :t
                                            :desc "File tree"
                                            :action ":Neotree toggle"}
                                           {:icon "󰦛"
                                            :key :s
                                            :desc "Restore session"
                                            :action :session}
                                           {:icon "󰗼"
                                            :key :q
                                            :desc :Quit
                                            :action ":qa"}]}}
               :image {:enabled true}
               :indent {:enabled true
                        :char "│"
                        :animate {:enabled false}
                        :scope {:hl [:rainbowcol1
                                     :rainbowcol2
                                     :rainbowcol3
                                     :rainbowcol4
                                     :rainbowcol5
                                     :rainbowcol6]}}
               :input {:enabled true :icon "❯ "}
               :lazygit {:enabled true :theme {}}
               :notifier {:enabled true
                          :icons {:error (. (vim.fn.sign_getdefined :DiagnosticSignError)
                                            :text)
                                  :warn (. (vim.fn.sign_getdefined :DiagnosticSignWarn)
                                           :text)
                                  :info (. (vim.fn.sign_getdefined :DiagnosticSignInfo)
                                           :text)
                                  :debug "󰃤"
                                  :trace "󰅪"}}
               :picker {:prompt "❯ "
                        :ui_select true
                        :previewers {:diff {:builtin false :cmd [:delta]}}
                        :win {:input {:keys {:<a-h> (tx! :preview_scroll_up
                                                         {:mode [:i :n]})
                                             :<a-l> (tx! :preview_scroll_down
                                                         {:mode [:i :n]})
                                             :<Tab> (tx! :cycle_win
                                                         {:mode [:i :n]})
                                             :<Esc> (tx! :cancel
                                                         {:mode [:i :n]})}}
                              :preview {:keys {:<Tab> :cycle_win}}}
                        :icons {:files {:dir "󰉋"
                                        :dir_open "󰝰"
                                        :file "󰈔"}
                                :git {:commit "󰜘"
                                      :staged ""
                                      :added ""
                                      :deleted ""
                                      :ignored ""
                                      :modified ""
                                      :renamed ""
                                      :unmerged ""
                                      :untracked ""}
                                :diagnostics {:Error (. (vim.fn.sign_getdefined :DiagnosticSignError)
                                                        1 :text)
                                              :Warn (. (vim.fn.sign_getdefined :DiagnosticSignWarn)
                                                       1 :text)
                                              :Hint (. (vim.fn.sign_getdefined :DiagnosticSignHint)
                                                       1 :text)
                                              :Info (. (vim.fn.sign_getdefined :DiagnosticSignInfo)
                                                       1 :text)}
                                :lsp {:unavailable "󰍴"
                                      :enabled "󰔡"
                                      :disabled "󰔢"
                                      :attached "󱘖"}}}
               :profiler {:pick {:picker :trouble}
                          :icons {:time "󱎫 "
                                  :pct "󰏰 "
                                  :count "󰆙 "
                                  :require "󰇚 "
                                  :modname "󰅪 "
                                  :plugin "󰏓 "
                                  :autocmd "󱐋 "
                                  :file "󰈔 "
                                  :fn "󰊕 "
                                  :status "󱖫 "}}
               :quickfile {:enabled true}
               :statuscolumn {:enabled true :git {:patterns [:GitSign]}}
               :terminal {:win {:style :float :border :solid}}
               :words {:enabled true}}
        :init #(vim.api.nvim_create_autocmd :User
                                            {:pattern :VeryLazy
                                             :callback #(do
                                                          (fn _G.dd [...]
                                                            (_G.Snacks.debug.inspect ...))
                                                          (fn _G.bt []
                                                            (_G.Snacks.debug.backtrace))

                                                          (set vim.print _G.dd)
                                                          (set vim.ui.input
                                                               _G.Snacks.input))})})]
