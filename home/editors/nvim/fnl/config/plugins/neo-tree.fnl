(import-macros {: use! : key! : tx!} :config.macros)
(import-macros {: merge!} :hibiscus.core)

[(use! :nvim-neo-tree/neo-tree.nvim
       {:dependencies [(use! :nvim-lua/plenary.nvim)
                       (use! :MunifTanjim/nui.nvim)
                       (use! :s1n7ax/nvim-window-picker
                             {:opts {:highlights {:statusline {:unfocused {:fg _G.base00
                                                                           :bg _G.base0F}}}
                                     :show_prompt false}})]
        :keys [(key! :<leader>t "<cmd>Neotree toggle<cr>" {:desc "File tree"})]
        :cmd :Neotree
        :opts {:sources [:filesystem]
               :open_files_do_not_replace_types [:terminal
                                                 :trouble
                                                 :dap-repl
                                                 :dapui_console
                                                 :dapui_scopes
                                                 :dapui_breakpoints
                                                 :dapui_stacks
                                                 :dapui_watches]
               :filesystem {:follow_current_file {:enabled true}
                            :use_libuv_file_watcher true
                            :filtered_items {:visible true}}
               :window {:mappings {:l :set_root
                                   :h :navigate_up
                                   :<space> :none
                                   :Y (tx! #(vim.fn.setreg "+"
                                                           (: ($1.tree:get_node)
                                                              :get_id)
                                                           :c)
                                           {:desc "Copy Path to Clipboard"})}
                        :width 32}
               :default_component_configs {:modified {:symbol "󰆓"}
                                           :diagnostics {:symbols {:error (. (vim.fn.sign_getdefined :DiagnosticSignError)
                                                                             1
                                                                             :text)
                                                                   :warn (. (vim.fn.sign_getdefined :DiagnosticSignWarn)
                                                                            1
                                                                            :text)
                                                                   :info (. (vim.fn.sign_getdefined :DiagnosticSignInfo)
                                                                            1
                                                                            :text)
                                                                   :hint (. (vim.fn.sign_getdefined :DiagnosticSignHint)
                                                                            1
                                                                            :text)}
                                                         :highlights {:error (. (vim.fn.sign_getdefined :DiagnosticSignError)
                                                                                1
                                                                                :texthl)
                                                                      :warn (. (vim.fn.sign_getdefined :DiagnosticSignWarn)
                                                                               1
                                                                               :texthl)
                                                                      :info (. (vim.fn.sign_getdefined :DiagnosticSignInfo)
                                                                               1
                                                                               :texthl)
                                                                      :hint (. (vim.fn.sign_getdefined :DiagnosticSignHint)
                                                                               1
                                                                               :texthl)}}
                                           :name {:trailing_slash true}
                                           :git_status {:symbols {:added ""
                                                                  :modified ""
                                                                  :deleted ""
                                                                  :renamed ""
                                                                  :untracked ""
                                                                  :ignored ""
                                                                  :unstaged ""
                                                                  :staged ""
                                                                  :conflict ""}}}
               :event_handlers (merge! [{:event :neo_tree_buffer_enter
                                         :handler #(vim.cmd "highlight! Cursor blend=100")}
                                        {:event :neo_tree_buffer_leave
                                         :handler #(vim.cmd (.. "highlight! Cursor blend=0 guibg="
                                                                _G.base0F))}]
                                       (let [handler #(_G.Snacks.rename.on_rename_file $1.source
                                                                                       $1.destination)]
                                         [{:event :file_moved : handler}
                                          {:event :file_renamed : handler}]))}})]
