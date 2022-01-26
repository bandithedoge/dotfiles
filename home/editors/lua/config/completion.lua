local cmp = require "cmp"
cmp.setup {
    snippet = {
        expand = function(args)
            require("luasnip").lsp_expand(args.body)
        end,
    },
    completion = {
        autocomplete = false,
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
        { name = "crates" },
        { name = "emoji" },
        { name = "latex_symbols" },
        { name = "luasnip" },
        { name = "neorg" },
        { name = "nvim_lsp" },
        { name = "nvim_lua" },
        { name = "orgmode" },
        { name = "path" },
        { name = "treesitter" },
    },
    sorting = {
        comparators = {
            cmp.config.compare.exact,
            cmp.config.compare.offset,
            cmp.config.compare.score,
            require("cmp-under-comparator").under,
            cmp.config.compare.kind,
            cmp.config.compare.sort_text,
            cmp.config.compare.length,
            cmp.config.compare.order,
        },
    },
}

cmp.setup.cmdline(":", {
    sources = {
        { name = "cmdline" },
        { name = "path" },
    },
})

cmp.setup.cmdline("/", {
    { name = "buffer" },
    { name = "nvim_lsp_document_symbol" },
})
