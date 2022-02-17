-- nvim-lspconfig {{{
local lsp = require "lspconfig"
local servers = {
    "bashls",
    "clangd",
    "cssls",
    "gdscript",
    "gopls",
    "hls",
    "html",
    "jsonls",
    "pylsp",
    "rnix",
    "rust_analyzer",
    "solargraph",
}

local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

local capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())

for _, server in ipairs(servers) do
    lsp[server].setup {
        capabilities = capabilities,
        settings = {
            json = {
                schemas = require("schemastore").json.schemas(),
            },
            Lua = {
                runtime = {
                    version = "LuaJIT",
                    path = runtime_path,
                },
                diagnostics = {
                    globals = { "vim" },
                },
                workspace = {
                    library = vim.api.nvim_get_runtime_file("", true),
                    preloadFileSize = 500,
                },
            },
        },
    }
end
-- }}}

-- lspkind-nvim {{{
require("lspkind").init()
-- }}}

-- lsp_signature.nvim {{{
require("lsp_signature").setup {
    bind = true,
    handler_opts = {
        border = "solid",
    },
    hint_prefix = "漣",
    floating_window = false,
    floating_window_above_cur_line = true,
}
-- }}}

-- null-ls.nvim {{{
local null_ls = require "null-ls"
local b = null_ls.builtins
local f = b.formatting
local d = b.diagnostics
local a = b.code_actions

null_ls.setup()

null_ls.register {
    f.black,
    f.cabal_fmt,
    f.fish_indent,
    f.fixjson,
    f.isort,
    f.nimpretty,
    f.rubocop,
    f.shellharden,
    f.shfmt,
    f.stylua,
    f.trim_whitespace,
    f.prettier.with {
        extra_args = function(params)
            return params.options
                and params.options.tabSize
                and {
                    "--tab-width",
                    params.options.tabSize,
                }
        end,
    },

    d.markdownlint.with { command = "markdownlint-cli2" },
    d.shellcheck,
    d.statix,

    a.shellcheck,
    a.statix,
}
-- }}}
