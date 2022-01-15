local vim = _G.vim
local fterm_float = _G.fterm_float

local MiniBufremove = require "mini.bufremove"
local wk = require "which-key"
local t = require "telescope.builtin"
local fm = require "fm-nvim"

vim.api.nvim_set_keymap("n", "<BS>", ":WhichKey \\\\<cr>", { silent = true })

wk.setup {
    ignore_missing = true,
    icons = { separator = "ﰲ" },
}

wk.register {
    ["<leader>"] = {
        ["<space>"] = { "<cmd>Telescope commands<cr>", "Enter command" },
        f = {
            name = "+Find",
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
            o = {
                function()
                    t.oldfiles()
                end,
                "File history",
            },
            s = {
                function()
                    t.symbols()
                end,
                "Symbols",
            },
            f = {
                function()
                    fm.Lf()
                end,
                "Files",
            },
        },
        o = {
            name = "+Open",
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
        e = {
            function()
                require("expand_expr").expand()
            end,
            "Expand expression",
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

    ["\\"] = {
        f = {
            function()
                vim.lsp.buf.formatting()
            end,
            "Format file",
        },
        a = {
            function()
                t.lsp_code_actions()
            end,
            "Code actions",
        },
        d = {
            function()
                t.diagnostics()
            end,
            "Diagnostics",
        },
        r = {
            function()
                vim.lsp.buf.rename()
            end,
            "Rename",
        },
    },
}
