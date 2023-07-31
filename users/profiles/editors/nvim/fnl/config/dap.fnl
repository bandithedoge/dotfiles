(require-macros :hibiscus.vim)

(_G.use :mfussenegger/nvim-dap
        {:dependencies [(_G.use :rcarriga/nvim-dap-ui)]
         :keys [(_G.key :<localleader>dd #(let [dapui (require :dapui)] (dapui.toggle)) {:desc :Toggle})
                (_G.key :<localleader>de #(let [dapui (require :dapui)] (dapui.eval)) {:desc :Eval})
                (_G.key :<localleader>db :<cmd>DapToggleBreakpoint<cr>
                        {:desc :Breakpoint})
                (_G.key :<localleader>dc :<cmd>DapContinue<cr>
                        {:desc :Continue})
                (_G.key :<localleader>do :<cmd>DapStepOver<cr>
                        {:desc "Step over"})
                (_G.key :<localleader>di :<cmd>DapStepInto<cr>
                        {:desc "Step into"})]
         :config #(let [dapui (require :dapui)
                        dap (require :dap)]
                    (set dap.listeners.after.event_initialized.dapui_config
                         dapui.open)
                    (set dap.listeners.after.event_terminated.dapui_config
                         dapui.close)
                    (set dap.listeners.after.event_exited.dapui_config
                         dapui.close)
                    (dapui.setup))})
