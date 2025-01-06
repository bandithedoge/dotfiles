(import-macros {: use! : key!} :config.macros)

[(use! :mfussenegger/nvim-dap
       {:dependencies [(use! :nvim-lua/plenary.nvim)
                       (use! :rcarriga/nvim-dap-ui
                             {:dependencies [(use! :nvim-neotest/nvim-nio)]})
                       (use! :theHamsta/nvim-dap-virtual-text {:config true})
                       (use! :folke/neoconf.nvim)]
        :keys [(key! :<localleader>dd
                     #(let [dapui (require :dapui)] (dapui.toggle))
                     {:desc :Toggle})
               (key! :<localleader>de
                     #(let [dapui (require :dapui)] (dapui.eval)) {:desc :Eval})
               (key! :<localleader>db :<cmd>DapToggleBreakpoint<cr>
                     {:desc :Breakpoint})
               (key! :<localleader>dc :<cmd>DapContinue<cr> {:desc :Continue})
               (key! :<localleader>do :<cmd>DapStepOver<cr> {:desc "Step over"})
               (key! :<localleader>di :<cmd>DapStepInto<cr> {:desc "Step into"})
               (key! :<localleader>dp #(let [dap (require :dap)] (dap.pause))
                     {:desc :Pause})
               (key! :<localleader>dt :<cmd>DapTerminate<cr> {:desc :Terminate})]
        :config #(let [dapui (require :dapui)
                       dap (require :dap)
                       vscode (require :dap.ext.vscode)
                       utils (require :dap.utils)
                       json (require :plenary.json)]
                   (dapui.setup {})
                   (set dap.listeners.after.event_initialized.dapui_config
                        #(dapui.open {}))
                   (set dap.listeners.after.event_terminated.dapui_config
                        #(dapui.close {}))
                   (set dap.listeners.after.event_exited.dapui_config
                        #(dapui.close {}))
                   (set vscode.json_decode
                        #(vim.json.decode (json.json_strip_comments $1)))
                   (vscode.load_launchjs nil
                                         {:node [:javascriptreact
                                                 :typescriptreact
                                                 :typescript
                                                 :javascript]
                                          :pwa-node [:javascriptreact
                                                     :typescriptreact
                                                     :typescript
                                                     :javascript]})
                   (when (vim.fn.filereadable :.vscode/launch.json)
                     (vscode.load_launchjs))
                   (set dap.adapters.codelldb
                        {:type :server
                         ;:host :localhost
                         :port "${port}"
                         :executable {:command :codelldb
                                      :args [:--port "${port}"]}})
                   (each [_ lang (ipairs [:c :cpp :rust :zig])]
                     (tset dap.configurations lang
                           [{:type :codelldb
                             :request :launch
                             :name "Launch file"
                             :program #(vim.fn.input "Path to executable: "
                                                     (.. (vim.fn.getcwd) "/")
                                                     :file)
                             :cwd "${workspaceFolder}"}
                            {:type :codelldb
                             :request :attach
                             :name "Attach to process"
                             :pid utils.pick_process
                             :cwd "${workspaceFolder}"}])))})]
