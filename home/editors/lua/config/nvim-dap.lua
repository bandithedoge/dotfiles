require("dapui").setup()
local dap = require "dap"

dap.adapters.ruby = {
    type = "executable",
    command = "readapt",
    args = { "stdio" },
}
dap.configurations.ruby = {
    {
        type = "ruby",
        request = "launch",
        useBundler = false,
    },
}
