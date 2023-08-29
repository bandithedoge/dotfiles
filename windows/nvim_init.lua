USING_NIX = false

local nvimpath = vim.fn.stdpath "config"

vim.opt.rtp:prepend(nvimpath)

local rice = vim.json.decode(io.open(vim.fn.expand "~/dotfiles/rice.json"):read "*a")
for k, v in pairs(rice) do
  _G[k] = v
end

local function bootstrap(plugin)
  local path = vim.fn.stdpath "data" .. "/lazy/" .. string.match(plugin, "/(.+)")

  if not vim.loop.fs_stat(path) then
    print("Installing " .. plugin .. "...")
    vim.fn.system {
      "git",
      "clone",
      "-c",
      "core.symlinks=true",
      "https://github.com/" .. plugin,
      path,
    }
  end

  vim.opt.rtp:prepend(path)
end

bootstrap "udayvir-singh/tangerine.nvim"
bootstrap "udayvir-singh/hibiscus.nvim"
bootstrap "folke/lazy.nvim"
bootstrap "EdenEast/nightfox.nvim"

vim.loader.enable()

require("tangerine").setup {
  target = vim.fn.stdpath "cache" .. "/tangerine",
  compiler = {
    hooks = { "oninit" },
  },
}

require "config"
