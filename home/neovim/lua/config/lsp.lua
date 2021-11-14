-- nvim-lspconfig {{{
local lsp = require "lspconfig"
local servers = {
    "rust_analyzer",
    "pylsp",
    "gdscript",
    "bashls",
    "html",
    "cssls",
    "jsonls",
    "clangd",
    "solargraph",
    "hls",
    "rnix",
}

local capabilities = require("cmp_nvim_lsp").update_capabilities(
    vim.lsp.protocol.make_client_capabilities()
)

for _, server in ipairs(servers) do
    lsp[server].setup {
        capabilities = capabilities,
    }
end
-- }}}

-- trouble.nvim {{{
require("trouble").setup {}
-- }}}

-- lspsaga.nvim {{{
require("lspsaga").init_lsp_saga()

vim.api.nvim_set_keymap(
    "n",
    "<C-j>",
    ":lua require('lspsaga.action').smart_scroll_with_saga(1)<cr>",
    { silent = true }
)
vim.api.nvim_set_keymap(
    "n",
    "<C-k>",
    ":lua require('lspsaga.action').smart_scroll_with_saga(-1)<cr>",
    { silent = true }
)
-- }}}

-- lspkind-nvim {{{
require("lspkind").init()
-- }}}

-- nvim-lightbulb {{{
vim.cmd [[autocmd CursorHold,CursorHoldI * lua require('nvim-lightbulb').update_lightbulb()]]
-- }}}