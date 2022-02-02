local MiniBufremove = require "mini.bufremove"
local wk = require "which-key"
local t = require "telescope.builtin"
local fm = require "fm-nvim"
local lsp = vim.lsp.buf

vim.api.nvim_set_keymap("n", "<cr>", ":noh<cr>", { silent = true })
vim.api.nvim_set_keymap("n", "<s-tab>", "zA", { silent = true })
vim.api.nvim_set_keymap("n", "<tab>", "za", { silent = true })
vim.api.nvim_set_keymap("n", "j", "gj", { silent = true })
vim.api.nvim_set_keymap("n", "k", "gk", { silent = true })
vim.api.nvim_set_keymap("n", "<BS>", ":WhichKey \\<cr>", { noremap = true, silent = true })

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

wk.setup {
    ignore_missing = true,
    icons = { separator = "" },
}

wk.register {
    ["<leader>"] = {
        ["<space>"] = { "<cmd>Telescope commands<cr>", "Enter command" },
        f = {
            name = "Find",
            t = {
                function()
                    t.builtin()
                end,
                "Telescope",
            },
            h = {
                function()
                    t.help_tags()
                end,
                "Help",
            },
            H = {
                function()
                    t.highlights()
                end,
                "Highlight groups",
            },
            f = {
                function()
                    require("telescope").extensions.frecency.frecency()
                end,
                "Files",
            },
        },
        o = {
            name = "Open",
            d = { "<cmd>:cd ~/dotfiles/<cr>", "Dotfiles" },
            s = { "<cmd>:cd ~/sql/<cr>", "School" },
            g = { "<cmd>:cd ~/git/<cr>", "Git" },
        },
        t = {
            function()
                require("nvim-tree").toggle()
            end,
            "File tree",
        },
        g = {
            function()
                fm.Lazygit()
            end,
            "Git",
        },
        T = {
            function()
                fterm_float:toggle()
            end,
            "Terminal",
        },
        F = {
            function()
                fm.Lf()
            end,
            "File explorer",
        },
        -- window/buffer management
        b = {
            function()
                t.buffers()
            end,
            "Buffers",
        },
        w = {
            function()
                MiniBufremove.delete()
            end,
            "Close buffer",
        },
        ["<C-w>"] = {
            function()
                MiniBufremove.delete(0, true)
            end,
            "Close buffer (force)",
        },
        W = { "<cmd>:close<cr>", "Close window" },
        n = { "<cmd>:new<cr>", "New window (horizontal)" },
        N = { "<cmd>:vnew<cr>", "New window (vertical)" },
    },

    ["<localleader>"] = {
        a = {
            function()
                lsp.code_action()
            end,
            "Code actions",
        },
        d = {
            function()
                t.diagnostics()
            end,
            "Diagnostics",
        },
        D = {
            function()
                lsp.definition()
            end,
            "Definition",
        },
        e = {
            function()
                require("expand_expr").expand()
            end,
            "Expand expression",
        },
        f = {
            function()
                lsp.formatting()
            end,
            "Format file",
        },
        h = {
            function()
                lsp.hover()
            end,
            "Hover",
        },
        r = {
            function()
                lsp.rename()
            end,
            "Rename",
        },
        s = {
            function()
                lsp.document_symbol()
            end,
            "Symbols",
        },
    },
}
