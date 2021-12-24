local wk = require "which-key"
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
            f = { "<cmd>lua require('telescope.builtin').file_browser({hidden = true})<cr>", "File" },
            t = { "<cmd>Telescope<cr>", "Telescope" },
            h = { "<cmd>Telescope help_tags<cr>", "Help" },
            H = { "<cmd>Telescope highlights<cr>", "Highlight groups" },
            c = { "<cmd>Telescope command_history<cr>", "Command history" },
            o = { "<cmd>Telescope oldfiles<cr>", "File history" },
            s = { "<cmd>Telescope symbols<cr>", "Symbols" },
            r = { "<cmd>Telescope reloader<cr>", "Reload module" },
        },
        o = {
            name = "+Open",
            d = { "<cmd>:cd ~/dotfiles/<cr>", "Dotfiles" },
            s = { "<cmd>:cd ~/sql/<cr>", "School" },
            g = { "<cmd>:cd ~/git/<cr>", "Git" },
        },
        t = { "<cmd>CHADopen<cr>", "File tree" },
        g = { "<cmd>:Neogit<cr>", "Git" },
        T = { "<cmd>:lua fterm_float:toggle()<cr>", "Terminal" },
        -- window/buffer management
        b = { "<cmd>Telescope buffers<cr>", "Buffers" },
        w = { "<cmd>:bd<cr>", "Close buffer" },
        W = { "<cmd>:close<cr>", "Close window" },
        n = { "<cmd>:new<cr>", "New window (horizontal)" },
        N = { "<cmd>:vnew<cr>", "New window (vertical)" },
    },

    ["\\"] = {
        f = { "<cmd>:lua vim.lsp.buf.formatting()<cr>", "Format file" },
        t = { "<cmd>TroubleToggle<cr>", "Trouble" },
        a = { "<cmd>Telescope lsp_code_actions<cr>", "Code actions" },
        d = { "<cmd>Telescope lsp_definitions<cr>", "Definition" },
        D = { "<cmd>Telescope lsp_type_definitions<cr>", "Type definition" },
        i = { "<cmd>Telescope lsp_implementations<cr>", "Implementation" },
        r = { "<cmd>Lspsaga rename<cr>", "Rename" },
        h = { "<cmd>Lspsaga hover_doc<cr>", "Documentation" },
        s = { "<cmd>Lspsaga signature_help<cr>", "Signature" },
        p = { "<cmd>Lspsaga preview_definition<cr>", "Preview definition" },
    },
}
