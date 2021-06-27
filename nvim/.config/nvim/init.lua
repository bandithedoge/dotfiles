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

vim.o.guifont = "FiraCode Nerd Font:h16"
vim.api.nvim_set_keymap('n', "<C-ScrollWheelUp>", ":set guifont=+<cr>", { silent = true })
vim.api.nvim_set_keymap('n', "<C-ScrollWheelDown>", ":set guifont=-<cr>", { silent = true })

if vim.fn.exists("g:fvim_loaded") then
    vim.cmd([[
        FVimCursorSmoothMove v:true
        FVimCursorSmoothBlink v:true
        FVimUIPopupMenu v:false
    ]])
end
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
        requires = { {'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'} },
        config = function()
            require("telescope").setup {
                defaults = {
                    prompt_prefix = " "
                },
                pickers = {
                    commands = {
                        theme = "ivy"
                    }
                }
            }
        end
    }
    -- }}}
    -- treefuckingsitter mate {{{
    use { 'nvim-treesitter/nvim-treesitter', run = ":TSUpdate",
        config = function()
            require('nvim-treesitter.configs').setup {
                ensure_installed = "lua",
                highlight = { enable = true }
            } end
        }
    -- }}}
    -- statusline {{{
    use { 'hoob3rt/lualine.nvim',
        requires = {'kyazdani42/nvim-web-devicons', opt = true},
        config = function()
            require('lualine').setup{
                options = {
                    theme = 'onedark',
                    component_separators = "",
                    section_separators = "",
                    extensions = { "nvim-tree" }
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
    -- comments {{{
    use 'b3nj5m1n/kommentary'
    -- }}}
    -- autopairs {{{
    use { 'windwp/nvim-autopairs', config = function()
        require('nvim-autopairs').setup()
    end }
    -- }}}
    -- discord rich presence {{{
    use 'andweeb/presence.nvim'
    -- }}}
    -- scrollbar {{{
    use 'dstein64/nvim-scrollview'
    -- }}}
    -- git {{{
    use 'TimUntersberger/neogit'
    -- }}}
    -- window visibility {{{
    use { 'sunjon/shade.nvim',
        config = require("shade").setup() }
    -- }}}
    -- autocompletion {{{
        use { 'hrsh7th/nvim-compe',
            config = function()
                require('compe').setup({
                    enabled = true,
                    autocomplete = true,
                    debug = false,
                    min_length = 2,
                    preselect = 'enable',
                    throttle_time = 80,
                    source_timeout = 200,
                    incomplete_delay = 400,
                    max_abbr_width = 100,
                    max_kind_width = 100,
                    max_menu_width = 100,
                    documentation = true,
                    source = {
                        path = true,
                        buffer = true,
                        calc = true,
                        nvim_lsp = true,
                        nvim_lua = true,
                        spell = true,
                        tags = true,
                        treesitter = true,
                        vsnip = false
                    }
                })
                local t = function(str)
                    return vim.api.nvim_replace_termcodes(str, true, true, true)
                end

                local check_back_space = function()
                    local col = vim.fn.col('.') - 1
                    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
                        return true
                    else
                        return false
                    end
                end

                -- Use (s-)tab to:
                --- move to prev/next item in completion menuone
                --- jump to prev/next snippet's placeholder
                _G.tab_complete = function()
                    if vim.fn.pumvisible() == 1 then
                        return t "<C-n>"
                    elseif check_back_space() then
                        return t "<Tab>"
                    else
                        return vim.fn['compe#complete']()
                    end
                end
                _G.s_tab_complete = function()
                    if vim.fn.pumvisible() == 1 then
                        return t "<C-p>"
                    else
                        -- If <S-Tab> is not working in your terminal, change it to <C-h>
                        return t "<S-Tab>"
                    end
                end

                vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
                vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
                vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
                vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
            end}
    -- }}}
    -- lsp {{{
    use { 'neovim/nvim-lspconfig',
        config = function()
            local lsp = require('lspconfig')
            lsp.rls.setup{}
            lsp.pyls.setup{}
            lsp.gdscript.setup{}
            lsp.bashls.setup{}
            lsp.html.setup{}
            lsp.cssls.setup{}
            lsp.nimls.setup{}
            lsp.jsonls.setup{}
            lsp.clangd.setup{}
        end}
    -- }}}
    -- keybindings {{{
    use { 'folke/which-key.nvim',
        config = function() 
            require("which-key").register({
                ["<space>"] = { "<cmd>Telescope commands<cr>", "Enter command" },
                f = {
                    name = "+File",
                    f = { "<cmd>Telescope file_browser<cr>", "Find" },
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
                t = { "<cmd>NvimTreeToggle<cr>", "File tree" },
                g = { "<cmd>Neogit<cr>", "Git" },
                h = { "<cmd>Telescope help_tags<cr>", "Help" }
            }, { prefix = "<leader>" })
        end }
    -- }}}
end)
-- }}}
