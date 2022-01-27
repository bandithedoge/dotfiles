local vim = _G.vim
vim.defer_fn(function()
    vim.g.mapleader = " "
    vim.o.clipboard = vim.o.clipboard .. "unnamedplus"
    vim.o.completeopt = "menu,menuone,noinsert"
    vim.o.conceallevel = 2
    vim.o.hidden = true
    vim.o.linebreak = true
    vim.o.mouse = "a"
    vim.o.redrawtime = 10000
    vim.o.termguicolors = true
    vim.opt.number = true
    vim.opt.relativenumber = true
    vim.opt.scrolloff = 4
    vim.opt.showmode = false
    vim.opt.showmode = false
    vim.opt.splitright = true
    vim.opt.timeoutlen = 0
    vim.wo.cursorline = true

    vim.opt.copyindent = true
    vim.opt.expandtab = true
    vim.opt.preserveindent = true
    vim.opt.shiftwidth = 4
    vim.opt.smartindent = true
    vim.opt.softtabstop = 4
    vim.opt.tabstop = 4

    vim.cmd [[au FileType nix,norg,CHADTree :setlocal shiftwidth=2]]
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

    require "config.colors"
    require "config.completion"
    require "config.dap"
    require "config.keybindings"
    require "config.langspec"
    require "config.lsp"
    require "config.snippets"
    require "config.treesitter"
    require "config.ui"
    require "config.utilities"
    require "config.writing"
end, 70)
