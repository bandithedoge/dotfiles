(import-macros {: tx!} :config.macros)
(import-macros {: map!} :hibiscus.vim)

(fn _G.dd [...])

(let [snacks (require :snacks)]
  (snacks.setup {:styles {:dashboard {:wo {:winhighlight "CursorLine:Normal,Normal:NormalNC,MiniTrailspace:NormalNC"}}
                          :input {:border :solid
                                  :relative :cursor
                                  :row -3
                                  :col 0}
                          :notification {:border :solid :wo {:winblend 0}}
                          :float {:backdrop 100}}
                 :bigfile {:enabled true}
                 :dashboard {:enabled true
                             :sections [{:section :keys :gap 1 :padding 1}
                                        {:section :recent_files :padding 1}]
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
                                    :info (. (vim.fn.sign_getdefined :DiagnosticSignInfo)
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
                                                          1 :text)}
                                  :lsp {:unavailable "󰍴"
                                        :enabled "󰔡"
                                        :disabled "󰔢"
                                        :attached "󱘖"}
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
                 :words {:enabled true}}))

(set vim.print _G.dd)
(set vim.ui.input _G.Snacks.input)

(map! [:n] :<leader>G _G.Snacks.gitbrowse.open "Open repo in browser")
(map! [:n] :<leader>T _G.Snacks.terminal.toggle :Terminal)
(map! [:n] :<leader>g _G.Snacks.lazygit.open :Git)

(map! [:n] :<leader><space> #(_G.Snacks.picker.commands {:layout :ivy})
      :Commands)

(map! [:n] :<leader>b #(_G.Snacks.picker.buffers {:layout :ivy}) :Buffers)
(map! [:n] :<leader>i #(_G.Snacks.picker.icons {:layout :select})
      "Insert symbol")

(map! [:n] :<leader>n _G.Snacks.picker.notifications :Notifications)
(map! [:n] :<leader>fF _G.Snacks.picker.recent "Recent files")
(map! [:n] :<leader>fH _G.Snacks.picker.highlights "Highlight groups")
(map! [:n] :<leader>ff _G.Snacks.picker.files :Files)
(map! [:n] :<leader>fg _G.Snacks.picker.grep :Grep)
(map! [:n] :<leader>fh _G.Snacks.picker.help :Help)
(map! [:n] :<leader>fk _G.Snacks.picker.keymaps :Keymaps)
(map! [:n] :<leader>fm _G.Snacks.picker.man :Man)
(map! [:n] :<leader>fp _G.Snacks.picker.pickers :Pickers)
(map! [:n] :<leader>fu _G.Snacks.picker.undo :Undo)
