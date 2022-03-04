(let [dapui (require :dapui)
      dap (require :dap)]
  (set dap.adapters
       {:ruby {:type :executable :command :readapt :args [:stdio]}})
  (set dap.configurations
       {:ruby [{:type :ruby :request :launch :useBundler false}]}))
