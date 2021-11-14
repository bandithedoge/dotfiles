local wk = require "which-key"
vim.api.nvim_set_keymap("n", "<BS>", ":WhichKey \\\\<cr>", { silent = true })

wk.setup {
    ignore_missing = true,
    icons = { separator = "ﰲ" },
}

wk.register {
    ["<leader>"] = {
        ["<space>"] = { "<cmd>Telescope commands<cr>", "Enter command" },
        p = {
            name = "+Packer",
            s = { "<cmd>PackerSync<cr>", "Sync" },
            c = { "<cmd>PackerCompile<cr>", "Compile" },
            C = { "<cmd>PackerClean<cr>", "Clean" },
            i = { "<cmd>PackerInstall<cr>", "Install" },
            p = { "<cmd>PackerProfile<cr>", "Profile" },
        },
        f = {
            name = "+Find",
            f = { "<cmd>Telescope file_browser<cr>", "File" },
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
            d = { "<cmd>:cd ~/dotfiles/<cr> <cmd>:NvimTreeOpen<cr>", "Dotfiles" },
            s = { "<cmd>:cd ~/sql/<cr> <cmd>:NvimTreeOpen<cr>", "School" },
        },
        t = { "<cmd>NvimTreeToggle<cr>", "File tree" },
        g = { "<cmd>LazyGit<cr>", "Git" },
        -- window/buffer management
        b = { "<cmd>Telescope buffers<cr>", "Buffers" },
        w = { "<cmd>:bd<cr>", "Close buffer" },
        W = { "<cmd>:close<cr>", "Close window" },
        n = { "<cmd>:new<cr>", "New window (horizontal)" },
        N = { "<cmd>:vnew<cr>", "New window (vertical)" },
    },

    ["\\"] = {
        f = { "<cmd>Format<cr>", "Format file" },
        t = { "<cmd>TroubleToggle<cr>", "Trouble" },
        r = { "<cmd>Lspsaga rename<cr>", "Rename" },
        h = { "<cmd>Lspsaga hover_doc<cr>", "Documentation" },
        s = { "<cmd>Lspsaga signature_help<cr>", "Signature" },
        p = { "<cmd>Lspsaga preview_definition<cr>", "Preview definition" },
    },
}