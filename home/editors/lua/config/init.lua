vim.defer_fn(function()
    vim.g.mapleader = " "
    vim.o.breakindent = true
    vim.o.clipboard = vim.o.clipboard .. "unnamedplus"
    vim.o.completeopt = "menu,menuone,noinsert,noselect,preview"
    vim.o.conceallevel = 2
    vim.o.fillchars = "fold: ,foldopen:▾,foldclose:▸,eob: "
    vim.o.hidden = true
    vim.o.inccommand = "split"
    vim.o.linebreak = true
    vim.o.mouse = "a"
    vim.o.number = true
    vim.o.path = "**"
    vim.o.redrawtime = 10000
    vim.o.relativenumber = true
    vim.o.scrolloff = 4
    vim.o.showmode = false
    vim.o.showmode = false
    vim.o.signcolumn = "auto:3"
    vim.o.splitbelow = true
    vim.o.splitright = true
    vim.o.termguicolors = true
    vim.o.timeoutlen = 0
    vim.o.updatetime = 200
    vim.opt.shortmess:append "atcsqS"
    vim.wo.cursorline = true
    vim.wo.foldmethod = "marker"

    vim.o.copyindent = true
    vim.o.expandtab = true
    vim.o.preserveindent = true
    vim.o.shiftwidth = 4
    vim.o.smartindent = true
    vim.o.softtabstop = 4
    vim.o.tabstop = 4

    vim.api.nvim_set_keymap("n", "<cr>", ":noh<cr>", { silent = true })
    vim.api.nvim_set_keymap("n", "<s-tab>", "zA", { silent = true })
    vim.api.nvim_set_keymap("n", "<tab>", "za", { silent = true })
    vim.api.nvim_set_keymap("n", "j", "gj", { silent = true })
    vim.api.nvim_set_keymap("n", "k", "gk", { silent = true })

    vim.cmd [[au FileType nix,norg,CHADTree :setlocal shiftwidth=2]]
    vim.cmd [[au BufReadPre *.nfo :setlocal fileencodings=cp437,utf-8]]

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
