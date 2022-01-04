local cmp = require "cmp"
cmp.setup {
    snippet = {
        expand = function(args)
            require("luasnip").lsp_expand(args.body)
        end,
    },
    completion = {
        completeopt = "menu,menuone,noinsert",
    },
    preselect = cmp.PreselectMode.None,
    mapping = {
        ["<C-Space>"] = cmp.mapping.complete(),
        ["<CR>"] = cmp.mapping.confirm { select = true },
        ["<C-j>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Select },
        ["<C-k>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Select },
        ["<C-h>"] = cmp.mapping.scroll_docs(-4),
        ["<C-l>"] = cmp.mapping.scroll_docs(4),
    },
    formatting = {
        format = require("lspkind").cmp_format {
            with_text = true,
        },
    },
    sources = {
        { name = "orgmode" },
        { name = "nvim_lsp" },
        { name = "path" },
        { name = "nvim_lua" },
        { name = "treesitter" },
        { name = "neorg" },
        { name = "luasnip" },
        { name = "latex_symbols" },
    },
    experimental = {
        ghost_text = true,
        native_menu = true,
    },
}
