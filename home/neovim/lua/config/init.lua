vim.defer_fn(function()
    vim.o.clipboard = vim.o.clipboard .. "unnamedplus"
    vim.o.mouse = "a"
    vim.o.hidden = true
    vim.opt.relativenumber = true
    vim.wo.cursorline = true
    vim.o.termguicolors = true
    vim.g.mapleader = " "
    vim.opt.timeoutlen = 0
    vim.o.redrawtime = 10000
    vim.o.linebreak = true
    vim.o.conceallevel = 2

    vim.opt.expandtab = true
    vim.opt.shiftwidth = 4
    vim.opt.tabstop = 4
    vim.opt.softtabstop = 4

    vim.cmd [[au FileType nix,norg :setlocal shiftwidth=2]]
    vim.cmd [[au BufReadPre *.nfo :setlocal fileencodings=cp437,utf-8]]

    vim.wo.foldmethod = "marker"
    vim.wo.foldtext = [[substitute(getline(v:foldstart),'\\t',repeat('\ ',&tabstop),'g').'  ' ]]
    vim.wo.fillchars = "fold: "
    vim.wo.foldnestmax = 3
    vim.wo.foldminlines = 1

    vim.api.nvim_set_keymap("n", "<cr>", ":noh<cr>", { silent = true })
    vim.api.nvim_set_keymap("n", "<tab>", "za", { silent = true })
    vim.api.nvim_set_keymap("n", "<s-tab>", "zA", { silent = true })
    vim.api.nvim_set_keymap("n", "j", "gj", { silent = true })
    vim.api.nvim_set_keymap("n", "k", "gk", { silent = true })

    vim.cmd [[ colorscheme blueballs ]]

    require "config.treesitter"
    require "config.utilities"
    require "config.ui"
    require "config.keybindings"
    require "config.lsp"
    require "config.completion"
    require "config.nvim-dap"
    require "config.writing"
end, 70)
