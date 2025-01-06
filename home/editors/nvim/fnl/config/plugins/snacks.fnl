(import-macros {: use! : key!} :config.macros)

[(use! :folke/snacks.nvim
       {:lazy false
        :keys [(key! :<leader>G #(_G.Snacks.gitbrowse)
                     {:desc "Open repo in browser"})
               (key! :<leader>n #(_G.Snacks.notifier.show_history)
                     {:desc "Notification history"})
               (key! :<leader>T #(_G.Snacks.terminal.toggle) {:desc :Terminal})]
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
               :notifier {:enabled true
                          :icons {:error (. (vim.fn.sign_getdefined :DiagnosticSignError)
                                            :text)
                                  :warn (. (vim.fn.sign_getdefined :DiagnosticSignWarn)
                                           :text)
                                  :info (. (vim.fn.sign_getdefined :DiagnosticSignInfo)
                                           :text)
                                  :debug "󰃤"
                                  :trace "󰅪"}}
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
               :terminal {:win {:style :float}}
               :words {:enabled true}}
        :init #(do
                 (fn _G.dd [...] (_G.Snacks.debug.inspect ...))
                 (fn _G.bt [] (_G.Snacks.debug.backtrace))

                 (set vim.print _G.dd))})]
