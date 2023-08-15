USING_NIX = false

local nvimpath = vim.fn.stdpath "config"

vim.opt.rtp:prepend(nvimpath)

local rice = vim.json.decode(io.open(vim.fn.expand "~/dotfiles/rice.json"):read "*a")
for k, v in pairs(rice) do
  _G[k] = v
end

local function bootstrap(plugin, branch)
  local path = vim.fn.stdpath "data" .. "/lazy/" .. string.match(plugin, "/(.+)")

  if not vim.loop.fs_stat(path) then
    print("Installing " .. plugin .. "...")
    vim.fn.system { "git", "clone", "-c", "core.symlinks=true", "https://github.com/" .. plugin, branch and "--branch=" .. branch, path }
  end

  vim.opt.rtp:prepend(path)
end

bootstrap("rktjmp/hotpot.nvim", "v0.7.0")
bootstrap "udayvir-singh/hibiscus.nvim"
bootstrap "folke/lazy.nvim"
bootstrap "EdenEast/nightfox.nvim"

vim.loader.enable()

-- https://github.com/udayvir-singh/tangerine.nvim/issues/25

local hotpot = require "hotpot"

hotpot.setup {
  compiler = {
    modules = {
      correlate = true,
    },
    macros = {
      env = "_COMPILER",
      compilerEnv = _G,
      allowedGlobals = false,
    },
  },
}

-- https://github.com/rktjmp/hotpot.nvim/blob/master/COOKBOOK.md#compiling-ftplugins-and-similar
vim.api.nvim_create_autocmd("FileType", {
    callback = function()
        pcall(require, "ftplugin." .. vim.fn.expand "<amatch>")
    end,
    group = vim.api.nvim_create_augroup("hotpot-ft", {}),
})

require "config"
