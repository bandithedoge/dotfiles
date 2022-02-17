local MiniBufremove = require "mini.bufremove"
local wk = require "which-key"

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

vim.keymap.set("n", "<cr>", ":noh<cr>", { silent = true })
vim.keymap.set("n", "<s-tab>", "zA", { silent = true })
vim.keymap.set("n", "<tab>", "za", { silent = true })
vim.keymap.set("n", "j", "gj", { silent = true })
vim.keymap.set("n", "k", "gk", { silent = true })

wk.setup {
    ignore_missing = true,
    icons = { separator = "" },
}

vim.keymap.set("n", "<BS>", ":WhichKey <localleader><cr>", { noremap = true, silent = true })

local t = require "telescope.builtin"
local fm = require "fm-nvim"
local lsp = vim.lsp.buf

wk.register {
    ["<leader>"] = {
        ["<space>"] = { "<cmd>Telescope commands<cr>", "Enter command" },
        f = {
            name = "Find",
            f = { require("telescope").extensions.frecency.frecency, "Files" },
            h = { t.help_tags, "Help" },
            H = { t.highlights, "Highlight groups" },
            t = { t.builtin, "Telescope" },
        },
        o = {
            name = "Open",
            d = { "<cmd>:cd ~/dotfiles/<cr>", "Dotfiles" },
            g = { "<cmd>:cd ~/git/<cr>", "Git" },
            s = { "<cmd>:cd ~/sql/<cr>", "School" },
        },
        b = { t.buffers, "Buffers" },
        F = { fm.Lf, "File explorer" },
        g = { fm.Lazygit, "Git" },
        n = { "<cmd>:new<cr>", "New window (horizontal)" },
        N = { "<cmd>:vnew<cr>", "New window (vertical)" },
        t = { require("nvim-tree").toggle, "File tree" },
        W = { "<cmd>:close<cr>", "Close window" },
        w = { MiniBufremove.delete, "Close buffer" },
        ["?"] = {"<cmd>:Cheatsheet<cr>", "Cheatsheet"},
        T = {
            function()
                fterm_float:toggle()
            end,
            "Terminal",
        },
        ["<C-w>"] = {
            function()
                MiniBufremove.delete(0, true)
            end,
            "Close buffer (force)",
        },
    },

    ["<localleader>"] = {
        a = { lsp.code_action, "Code actions" },
        D = { lsp.definition, "Definition" },
        d = { t.diagnostics, "Diagnostics" },
        e = { require("expand_expr").expand, "Expand expression" },
        f = { lsp.formatting, "Format file" },
        h = { lsp.hover, "Hover" },
        r = { lsp.rename, "Rename" },
        s = { lsp.document_symbol, "Symbols" },
    },
}
