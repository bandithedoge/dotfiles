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

-- lsp_signature.nvim {{{
require("lsp_signature").setup {
    bind = true,
    handler_opts = {
        border = "none",
    },
    hint_prefix = "漣",
}
-- }}}

-- null-ls.nvim {{{
local null_ls = require "null-ls"

null_ls.config {
    sources = {
        null_ls.builtins.formatting.black,
        null_ls.builtins.formatting.clang_format,
        null_ls.builtins.formatting.codespell,
        null_ls.builtins.formatting.fixjson,
        null_ls.builtins.formatting.isort,
        null_ls.builtins.formatting.nixfmt,
        null_ls.builtins.formatting.prettier.with { extra_args = { "--tab-width", "4" } },
        null_ls.builtins.formatting.rubocop,
        null_ls.builtins.formatting.rustfmt,
        null_ls.builtins.formatting.shfmt,
        null_ls.builtins.formatting.stylua,

        null_ls.builtins.diagnostics.codespell,
        null_ls.builtins.diagnostics.cppcheck,
        null_ls.builtins.diagnostics.flake8,
        null_ls.builtins.diagnostics.luacheck,
        null_ls.builtins.diagnostics.markdownlint.with { command = "markdownlint-cli2" },
        null_ls.builtins.diagnostics.rubocop,
        null_ls.builtins.diagnostics.shellcheck,
        null_ls.builtins.diagnostics.statix,
        null_ls.builtins.diagnostics.stylelint,
        null_ls.builtins.diagnostics.yamllint,

        null_ls.builtins.code_actions.gitsigns,
        null_ls.builtins.code_actions.shellcheck,
        null_ls.builtins.code_actions.statix,
    },
}

lsp["null-ls"].setup {}
-- }}}
