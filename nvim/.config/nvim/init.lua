-- packer stuff {{{
local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
    execute 'packadd packer.nvim'
end
-- }}}

-- general settings {{{
vim.o.clipboard = vim.o.clipboard .. 'unnamedplus'
vim.o.mouse = 'a'
vim.opt.relativenumber = true
vim.wo.cursorline = true
vim.o.termguicolors = true
vim.g.mapleader = " "
vim.wo.foldmethod = "marker"
vim.opt.timeoutlen = 300

vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
-- }}}

-- packages {{{
vim.cmd [[packadd packer.nvim]]
return require('packer').startup(function()
    use 'wbthomason/packer.nvim'
    -- color scheme {{{
    use { 'NTBBloodbath/doom-one.nvim',
        config = function() vim.cmd([[ colorscheme doom-one ]]) end }
    -- }}}
    -- fuzzy finder {{{
    use { 'nvim-telescope/telescope.nvim',
        requires = { {'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'} } }
    -- }}}
    -- treefuckingsitter mate {{{
    use { 'nvim-treesitter/nvim-treesitter',
        run = ":TSUpdate" }
    -- }}}
    -- statusline {{{
    use { 'hoob3rt/lualine.nvim',
        requires = {'kyazdani42/nvim-web-devicons', opt = true},
        config = function()
            require('lualine').setup{
                options = {
                    theme = 'onedark'
                }
            }
        end }
    -- }}}
    -- indent guides {{{
    use { 'glepnir/indent-guides.nvim',
        config = function() require('indent_guides').setup({
            indent_tab_guides = true
        })
        end }
    -- }}}
    -- bufferline {{{
    use { 'akinsho/nvim-bufferline.lua', requires = 'kyazdani42/nvim-web-devicons',
            config = function() require("bufferline").setup() end }
    -- }}}
    -- file explorer {{{
    use { 'kyazdani42/nvim-tree.lua', requires = 'kyazdani42/nvim-web-devicons' }
    -- }}}
    -- keybindings {{{
    use { 'folke/which-key.nvim',
        config = function() 
            require("which-key").register({
                ["<space>"] = { "<cmd>Telescope commands<cr>", "Enter command" },
                f = {
                    name = "+File",
                    f = { "<cmd>Telescope find_files<cr>", "Find" },
                    n = { "<cmd>enew<cr>", "New" },
                    c = { "<cmd>bd<cr>", "Close" },
                    C = { "<cmd>bd!<cr>", "Close without saving" },
                    b = { "<cmd>Telescope buffers<cr>", "Buffers" },
                    j = { "<cmd>BufferLineCycleNext<cr>", "Next buffer" },
                    k = { "<cmd>BufferLineCyclePrev<cr>", "Previous buffer" },
                    J = { "<cmd>BufferLineMoveNext<cr>", "Move buffer forwards" },
                    K = { "<cmd>BufferLineMovePrev<cr>", "Move buffer backwards" }
                },
                p = {
                    name = "+Packer",
                    s = { "<cmd>PackerSync<cr>", "Sync" },
                    c = { "<cmd>PackerCompile<cr>", "Compile" },
                    C = { "<cmd>PackerClean<cr>", "Clean" },
                    i = { "<cmd>PackerInstall<cr>", "Install" }
                },
                t = { "<cmd>NvimTreeToggle<cr>", "File tree" }
            }, { prefix = "<leader>" })
        end }
    -- }}}
end)
-- }}}
