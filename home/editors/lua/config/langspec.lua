-- crates.nvim {{{
require("crates").setup()
-- }}}

-- lua-dev.nvim {{{
require("lspconfig").sumneko_lua.setup(require("lua-dev").setup {
    runtime_path = true,
})
-- }}}
