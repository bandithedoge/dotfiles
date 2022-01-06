local vim = _G.vim
local wk = require "which-key"
local t = require "telescope.builtin"
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
        },
        o = {
            name = "+Open",
            d = { "<cmd>:cd ~/dotfiles/<cr>", "Dotfiles" },
            s = { "<cmd>:cd ~/sql/<cr>", "School" },
            g = { "<cmd>:cd ~/git/<cr>", "Git" },
        },
        t = { "<cmd>CHADopen<cr>", "File tree" },
        g = { "<cmd>:lua fterm_lazygit:toggle()<cr>", "Git" },
        T = { "<cmd>:lua fterm_float:toggle()<cr>", "Terminal" },
        -- window/buffer management
        b = {
            function()
                t.buffers()
            end,
            "Buffers",
        },
        w = { "<cmd>:bd<cr>", "Close buffer" },
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
        t = { "<cmd>TroubleToggle<cr>", "Trouble" },
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
        r = { "<cmd>Lspsaga rename<cr>", "Rename" },
        h = { "<cmd>Lspsaga hover_doc<cr>", "Documentation" },
        s = { "<cmd>Lspsaga signature_help<cr>", "Signature" },
        p = { "<cmd>Lspsaga preview_definition<cr>", "Preview definition" },
    },
}
