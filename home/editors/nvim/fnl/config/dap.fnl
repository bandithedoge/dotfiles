[(_G.use :mfussenegger/nvim-dap
         {:dependencies [(_G.use :nvim-lua/plenary.nvim)
                         (_G.use :rcarriga/nvim-dap-ui
                                 {:dependencies [(_G.use :nvim-neotest/nvim-nio)]})
                         (_G.use :theHamsta/nvim-dap-virtual-text
                                 {:config true})
                         (_G.use :folke/neoconf.nvim)]
          :keys [(_G.key :<localleader>dd
                         #(let [dapui (require :dapui)] (dapui.toggle))
                         {:desc :Toggle})
                 (_G.key :<localleader>de
                         #(let [dapui (require :dapui)] (dapui.eval))
                         {:desc :Eval})
                 (_G.key :<localleader>db :<cmd>DapToggleBreakpoint<cr>
                         {:desc :Breakpoint})
                 (_G.key :<localleader>dc :<cmd>DapContinue<cr>
                         {:desc :Continue})
                 (_G.key :<localleader>do :<cmd>DapStepOver<cr>
                         {:desc "Step over"})
                 (_G.key :<localleader>di :<cmd>DapStepInto<cr>
                         {:desc "Step into"})
                 (_G.key :<localleader>dp
                         #(let [dap (require :dap)] (dap.pause)) {:desc :Pause})
                 (_G.key :<localleader>dt :<cmd>DapTerminate<cr>
                         {:desc :Terminate})]
          :config #(let [dapui (require :dapui)
                         dap (require :dap)
                         vscode (require :dap.ext.vscode)
                         utils (require :dap.utils)
                         json (require :plenary.json)]
                     (dapui.setup)
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
                     (set dap.adapters.codelldb
                          {:type :server
                           ; :host :localhost
                           :port "${port}"
                           :executable {:command :codelldb
                                        :args [:--port "${port}"]}})
                     (set dap.configurations.c
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
                            :cwd "${workspaceFolder}"}])
                     (set dap.configurations.cpp dap.configurations.c)
                     (set dap.configurations.rust dap.configurations.c)
                     (set dap.configurations.zig dap.configurations.c))})]

