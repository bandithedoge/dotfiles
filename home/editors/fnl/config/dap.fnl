(let [dapui (require :dapui)
      dap (require :dap)]
  (set dap.listeners.after.event_initialized.dapui_config dapui.open)
  (set dap.listeners.after.event_terminated.dapui_config dapui.close)
  (set dap.listeners.after.event_exited.dapui_config dapui.close)
  (set dap.adapters
       {:ruby {:type :executable :command :readapt :args [:stdio]}
        :python {:type :executable
                 :command "~/.nix-profile/bin/python3.10"
                 :args [:-m :debugpy.adapter]}})
  (set dap.configurations
       {:ruby [{:type :ruby :request :launch :useBundler false}]
        :python [{:type :python
                  :request :launch
                  :name "Launch file"
                  :program "${file}"
                  :pythonPath "~/.nix-profile/bin/python3.10"}]})
  (dapui.setup))
