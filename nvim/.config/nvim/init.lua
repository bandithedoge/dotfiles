-- general settings {{{

vim.o.clipboard = vim.o.clipboard .. "unnamedplus"
vim.o.mouse = "a"
vim.opt.relativenumber = true
vim.wo.cursorline = true
vim.o.termguicolors = true
vim.g.mapleader = " "
vim.wo.foldmethod = "marker"
vim.opt.timeoutlen = 0
vim.o.redrawtime = 10000

vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.softtabstop = 4

vim.o.guifont = "FiraCode Nerd Font:h16"
vim.api.nvim_set_keymap("n", "<C-ScrollWheelUp>", ":set guifont=+<cr>", { silent = true })
vim.api.nvim_set_keymap("n", "<C-ScrollWheelDown>", ":set guifont=-<cr>", { silent = true })

vim.api.nvim_set_keymap("n", "<cr>", ":noh<cr>", { silent = true })
vim.api.nvim_set_keymap("n", "<tab>", "za", { silent = true })
vim.api.nvim_set_keymap("n", "<s-tab>", "zA", { silent = true })
-- }}}

-- packages {{{
-- packer {{{
local execute = vim.api.nvim_command
local fn = vim.fn
local install_path = fn.stdpath "data" .. "/site/pack/packer/opt/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
    fn.system { "git", "clone", "https://github.com/wbthomason/packer.nvim", install_path }
    execute "packadd packer.nvim"
end

local packer = require "packer"

packer.init {
    display = {
        error_sym = "",
    },
    profile = {
        enable = true,
    },
}

-- }}}

packer.startup(function()
    use "wbthomason/packer.nvim"
    -- ui {{{
    -- color scheme {{{
    use {
        "blueballs-theme/blueballs-neovim",
        config = function()
            vim.cmd [[ colorscheme blueballs ]]
        end,
    }
    -- }}}
    -- statusline {{{
    use {
        "hoob3rt/lualine.nvim",
        requires = "kyazdani42/nvim-web-devicons",
        config = function()
            require("lualine").setup {
                options = {
                    theme = "blueballs",
                    component_separators = "",
                    section_separators = "",
                },
                extensions = { "nvim-tree" },
                sections = {
                    lualine_a = { "mode" },
                    lualine_b = { { "filename", path = 1 } },
                    lualine_c = { "branch", "diff" },
                    lualine_x = { { "diagnostics", sources = { "nvim_lsp" } } },
                    lualine_y = { "encoding", "fileformat", "filetype" },
                    lualine_z = { "location", "progress" },
                },
            }
        end,
    }
    -- }}}
    -- indent guides {{{
    use {
        "glepnir/indent-guides.nvim",
        event = "BufRead",
        config = function()
            require("indent_guides").setup {
                indent_tab_guides = true,
            }
        end,
    }
    -- }}}
    -- bufferline {{{
    use {
        "akinsho/nvim-bufferline.lua",
        requires = "kyazdani42/nvim-web-devicons",
        config = function()
            require("bufferline").setup()
        end,
    }
    -- }}}
    -- scrollbar {{{
    use "dstein64/nvim-scrollview"
    -- }}}
    -- }}}

    -- text editing {{{
    -- treefuckingsitter mate {{{
    use {
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
        config = function()
            require("nvim-treesitter.configs").setup {
                ensure_installed = "lua",
                highlight = { enable = true },
                rainbow = { enable = true, extended_mode = true },
            }

            require("nvim-treesitter.parsers").get_parser_configs().norg = {
                install_info = {
                    url = "https://github.com/vhyrro/tree-sitter-norg",
                    files = { "src/parser.c" },
                    branch = "main",
                },
            }
        end,
    }

    use {
        { "p00f/nvim-ts-rainbow" },
        {
            "windwp/nvim-autopairs",
            config = function()
                require("nvim-autopairs").setup()
                require("nvim-autopairs.completion.compe").setup {
                    map_cr = true,
                    map_complete = true,
                }
            end,
        },
        requires = "nvim-treesitter/nvim-treesitter",
        after = "nvim-treesitter",
    }
    -- }}}
    -- lsp {{{
    use {
        "neovim/nvim-lspconfig",
        config = function()
            local lsp = require "lspconfig"

            lsp.rls.setup {}
            lsp.jedi_language_server.setup {}
            lsp.gdscript.setup {}
            lsp.bashls.setup {}
            lsp.html.setup {}
            lsp.cssls.setup {}
            lsp.nimls.setup {}
            lsp.jsonls.setup {}
            lsp.clangd.setup {}
            lsp.solargraph.setup {}
            lsp.hls.setup {}
        end,
    }

    use {
        {
            "folke/trouble.nvim",
            requires = "kyazdani42/nvim-web-devicons",
            config = function()
                require("trouble").setup {}
            end,
        },
        {
            "glepnir/lspsaga.nvim",
            config = function()
                require("lspsaga").init_lsp_saga()
            end,
        },
        {
            "onsails/lspkind-nvim",
            config = function()
                require("lspkind").init()
            end,
        },
        {
            "kosayoda/nvim-lightbulb",
            config = function()
                vim.cmd [[autocmd CursorHold,CursorHoldI * lua require('nvim-lightbulb').update_lightbulb()]]
            end,
        },
        {
            "ray-x/lsp_signature.nvim",
            config = function()
                require("lsp_signature").setup()
            end,
        },
        after = "nvim-lspconfig",
    }

    -- }}}
    -- comments {{{
    use "b3nj5m1n/kommentary"
    -- }}}
    -- autocompletion {{{
    use {
        "hrsh7th/nvim-compe",
        config = function()
            require("compe").setup {
                enabled = true,
                autocomplete = true,
                debug = false,
                min_length = 2,
                preselect = "enable",
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
                    spell = false,
                    tags = true,
                    treesitter = true,
                    vsnip = false,
                    orgmode = true,
                    neorg = true,
                },
            }
            local t = function(str)
                return vim.api.nvim_replace_termcodes(str, true, true, true)
            end

            local check_back_space = function()
                local col = vim.fn.col "." - 1
                if col == 0 or vim.fn.getline("."):sub(col, col):match "%s" then
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
                    return vim.fn["compe#complete"]()
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

            vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", { expr = true })
            vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", { expr = true })
            vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
            vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
        end,
    }
    -- }}}
    -- formatter {{{
    use {
        "mhartington/formatter.nvim",
        cmd = { "Format", "FormatWrite" },
        config = function()
            require("formatter").setup {
                filetype = {
                    lua = {
                        function()
                            return {
                                exe = "stylua",
                                args = { "-", "--search-parent-directories" },
                                stdin = true,
                            }
                        end,
                    },
                },
            }
        end,
    }
    -- }}}
    -- }}}

    -- utility {{{
    -- fuzzy finder {{{
    use {
        "nvim-telescope/telescope.nvim",
        requires = { { "nvim-lua/popup.nvim" }, { "nvim-lua/plenary.nvim" } },
        cmd = "Telescope",
        config = function()
            require("telescope").setup {
                defaults = {
                    prompt_prefix = " ",
                },
                pickers = {
                    commands = {
                        theme = "ivy",
                    },
                },
            }
        end,
    }

    use { { "sudormrfbin/cheatsheet.nvim" }, { "nvim-telescope/telescope-symbols.nvim" }, after = "telescope.nvim" }
    -- }}}
    -- git {{{
    -- use { 'TimUntersberger/neogit', cmd = "Neogit" }
    use { "kdheepak/lazygit.nvim", cmd = "LazyGit" }

    use {
        "lewis6991/gitsigns.nvim",
        requires = "nvim-lua/plenary.nvim",
        event = "BufRead",
        config = function()
            require("gitsigns").setup()
        end,
    }
    -- }}}
    -- file tree {{{
    use {
        "kyazdani42/nvim-tree.lua",
        requires = "kyazdani42/nvim-web-devicons",
        cmd = { "NvimTreeToggle", "NvimTreeClose", "NvimTreeOpen" },
        config = function()
            local tree_cb = require("nvim-tree.config").nvim_tree_callback

            vim.g.nvim_tree_auto_close = 1
            vim.g.nvim_tree_follow = 0
            vim.g.nvim_tree_indent_markers = 1
            vim.g.nvim_tree_highlight_opened_files = 1
            vim.g.nvim_tree_lsp_diagnostics = 1

            vim.g.nvim_tree_bindings = {
                { key = "h", cb = tree_cb "dir_up" },
                { key = "l", cb = tree_cb "cd" },
            }
        end,
    }
    -- }}}
    -- discord {{{
    use "andweeb/presence.nvim"
    -- }}}
    -- mkdir {{{
    use {
        "jghauser/mkdir.nvim",
        event = "BufWritePre",
        config = function()
            require "mkdir"
        end,
    }
    -- }}}
    -- }}}

    -- notes {{{
    -- neorg {{{
    use {
        "vhyrro/neorg",
        requires = { "nvim-lua/plenary.nvim" },
        branch = "unstable",
        ft = "norg",
        config = function()
            require("neorg").setup {
                load = {
                    ["core.defaults"] = {},
                    ["core.norg.concealer"] = {},
                },
            }
        end,
    }
    -- }}}
    -- heresy {{{
    use {
        "kristijanhusak/orgmode.nvim",
        ft = "org",
        config = function()
            require("orgmode").setup {
                org_hide_leading_stars = true,
            }
        end,
    }
    -- }}}
    -- }}}

    -- keybindings {{{
    use {
        "folke/which-key.nvim",
        config = function()
            local wk = require "which-key"
            vim.api.nvim_set_keymap("n", "<BS>", ":WhichKey \\\\<cr>", { silent = true })

            wk.setup {
                ignore_missing = true,
            }

            wk.register {
                ["<leader>"] = {
                    ["<space>"] = { "<cmd>Telescope commands<cr>", "Enter command" },
                    f = {
                        name = "+File",
                        f = { "<cmd>Telescope file_browser<cr>", "Find" },
                        n = { "<cmd>enew<cr>", "New" },
                        c = { "<cmd>bd<cr>", "Close" },
                        C = { "<cmd>bd!<cr>", "Close without saving" },
                        j = { "<cmd>BufferLineCycleNext<cr>", "Next buffer" },
                        k = { "<cmd>BufferLineCyclePrev<cr>", "Previous buffer" },
                        J = { "<cmd>BufferLineMoveNext<cr>", "Move buffer forwards" },
                        K = { "<cmd>BufferLineMovePrev<cr>", "Move buffer backwards" },
                    },
                    p = {
                        name = "+Packer",
                        s = { "<cmd>PackerSync<cr>", "Sync" },
                        c = { "<cmd>PackerCompile<cr>", "Compile" },
                        C = { "<cmd>PackerClean<cr>", "Clean" },
                        i = { "<cmd>PackerInstall<cr>", "Install" },
                        p = { "<cmd>PackerProfile<cr>", "Profile" },
                    },
                    t = {
                        name = "+Telescope",
                        t = { "<cmd>Telescope<cr>", "Telescope" },
                        h = { "<cmd>Telescope help_tags<cr>", "Help" },
                        H = { "<cmd>Telescope highlights<cr>", "Highlight groups" },
                        C = { "<cmd>Telescope command_history<cr>", "Command history" },
                        f = { "<cmd>Telescope oldfiles<cr>", "File history" },
                        s = { "<cmd>Telescope symbols", "Symbols" },
                    },
                    o = {
                        name = "+Open",
                        d = { "<cmd>:cd ~/dotfiles/<cr> <cmd>:e README.md<cr>", "Dotfiles" },
                        s = { "<cmd>:cd ~/sql/<cr> <cmd>:e README.md<cr>", "School" },
                    },
                    T = { "<cmd>NvimTreeToggle<cr>", "File tree" },
                    b = { "<cmd>Telescope buffers<cr>", "Buffers" },
                    g = { "<cmd>LazyGit<cr>", "Git" },
                },
                ["\\"] = {
                    h = { "<cmd>Lspsaga lsp_finder<cr>", "Cursor word reference" },
                    f = { "<cmd>Format<cr>", "Format file" },
                },
            }
        end,
    }
    -- }}}
end)

-- }}}
