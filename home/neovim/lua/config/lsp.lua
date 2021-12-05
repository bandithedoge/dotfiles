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
    "rnix",
}

local capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())

for _, server in ipairs(servers) do
    lsp[server].setup {
        capabilities = capabilities,
        settings = {
            json = {
                schemas = require("schemastore").json.schemas(),
            },
        },
    }
end
-- }}}

-- trouble.nvim {{{
require("trouble").setup {}
-- }}}

-- lspsaga.nvim {{{
require("lspsaga").init_lsp_saga()

vim.api.nvim_set_keymap("n", "<C-j>", ":lua require('lspsaga.action').smart_scroll_with_saga(1)<cr>", {
    silent = true,
})
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

-- nvim-lint {{{
require("lint").linters_by_ft = {
    lua = { "luacheck" },
    cpp = { "clangtidy" },
    nix = { "nix", "statix" },
    html = { "tidy" },
    markdown = { "markdownlint", "languagetool" },
    python = { "pylint" },
    sh = { "shellcheck" },
}

vim.cmd [[ au InsertLeave,BufWinEnter * :lua require("lint").try_lint() ]]
-- }}}
