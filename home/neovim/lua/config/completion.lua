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
        ["<C-CR>"] = cmp.mapping.confirm(),
        ["<C-j>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Select },
        ["<C-k>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Select },
        ["<C-h>"] = cmp.mapping.scroll_docs(-4),
        ["<C-l>"] = cmp.mapping.scroll_docs(4),
    },
    formatting = {
        format = function(entry, vim_item)
            vim_item.kind = require("lspkind").presets.default[vim_item.kind]
            return vim_item
        end,
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
}
