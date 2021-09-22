-- vim: fdm=marker
-- general settings {{{
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

vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4
vim.opt.softtabstop = 4

vim.wo.foldmethod = "expr"
vim.wo.foldexpr = "nvim_treesitter#foldexpr()"
vim.wo.foldtext = [[substitute(getline(v:foldstart),'\\t',repeat('\ ',&tabstop),'g').'...'.trim(getline(v:foldend)) ]]
vim.wo.fillchars = "fold: "
vim.wo.foldnestmax = 3
vim.wo.foldminlines = 1

vim.o.guifont = "FiraCode Nerd Font:h16"

vim.api.nvim_set_keymap("n", "<cr>", ":noh<cr>", { silent = true })
vim.api.nvim_set_keymap("n", "<tab>", "za", { silent = true })
vim.api.nvim_set_keymap("n", "<s-tab>", "zA", { silent = true })
vim.api.nvim_set_keymap("n", "j", "gj", { silent = true })
vim.api.nvim_set_keymap("n", "k", "gk", { silent = true })
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
        "lukas-reineke/indent-blankline.nvim",
        config = function()
            require("indent_blankline").setup {
                show_current_context = true,
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
    -- color highlights {{{
    use {
        "norcalli/nvim-colorizer.lua",
        config = function()
            require("colorizer").setup()
        end,
    }
    -- }}}
    -- close buffers {{{
    use "kazhala/close-buffers.nvim"
    -- }}}
    -- }}}

    -- text editing {{{
    -- treefuckingsitter mate {{{
    use {
        "nvim-treesitter/nvim-treesitter",
        run = ":TSUpdate",
        config = function()
            local parsers = require("nvim-treesitter.parsers").get_parser_configs()

            parsers.norg = {
                install_info = {
                    url = "https://github.com/vhyrro/tree-sitter-norg",
                    files = { "src/parser.c", "src/scanner.cc" },
                    branch = "main",
                },
            }

            require("nvim-treesitter.configs").setup {
                ensure_installed = { "lua", "norg" },
                highlight = { enable = true },
                rainbow = { enable = true, extended_mode = true },
            }

            require("nvim-treesitter.install").compilers = { "clang", "clang++" }
        end,
    }

    use {
        { "p00f/nvim-ts-rainbow" },
        {
            "windwp/nvim-autopairs",
            config = function()
                require("nvim-autopairs").setup()
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
            lsp.pylsp.setup {}
            lsp.gdscript.setup {}
            lsp.bashls.setup {}
            lsp.html.setup {}
            lsp.cssls.setup {}
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

                vim.api.nvim_set_keymap(
                    "n",
                    "<C-j>",
                    ":lua require('lspsaga.action').smart_scroll_with_saga(1)<cr>",
                    { silent = true }
                )
                vim.api.nvim_set_keymap(
                    "n",
                    "<C-k>",
                    ":lua require('lspsaga.action').smart_scroll_with_saga(-1)<cr>",
                    { silent = true }
                )
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
        after = "nvim-lspconfig",
    }

    -- }}}
    -- comments {{{
    use "b3nj5m1n/kommentary"
    -- }}}
    -- autocompletion {{{
    use {
        "hrsh7th/nvim-cmp",
        requires = {
            {
                "hrsh7th/cmp-nvim-lsp",
                "hrsh7th/cmp-path",
                "hrsh7th/cmp-nvim-lua",
                "ray-x/cmp-treesitter",
                { "L3MON4D3/LuaSnip", requires = "saadparwaiz1/cmp_luasnip" },
            },
            after = "nvim-cmp",
        },
        config = function()
            local cmp = require "cmp"
            cmp.setup {
                completion = {
                    completeopt = "menu,menuone,noinsert",
                },
                mapping = {
                    ["<C-Space>"] = cmp.mapping.complete(),
                    ["<C-j>"] = cmp.mapping.select_next_item(),
                    ["<C-k>"] = cmp.mapping.select_prev_item(),
                    ["<C-h>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-l>"] = cmp.mapping.scroll_docs(4),
                    ["<C-c>"] = cmp.mapping.close(),
                },
                formatting = {
                    format = function(entry, vim_item)
                        vim_item.kind = require("lspkind").presets.default[vim_item.kind]
                        return vim_item
                    end,
                },
                sources = {
                    { name = "orgmode" },
                    { name = "nvim_lsp" },
                    { name = "path" },
                    { name = "nvim_lua" },
                    { name = "treesitter" },
                    { name = "neorg" },
                    { name = "luasnip" },
                },
            }
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
                    python = {
                        function()
                            return {
                                exe = "yapf",
                                args = { "-i" },
                                stdin = false,
                            }
                        end,
                        function()
                            return {
                                exe = "isort",
                                stdin = false,
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

    use { "nvim-telescope/telescope-symbols.nvim", after = "telescope.nvim" }
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
        config = function()
            local tree_cb = require("nvim-tree.config").nvim_tree_callback

            vim.g.nvim_tree_auto_close = 0
            vim.g.nvim_tree_follow = 0
            vim.g.nvim_tree_indent_markers = 1
            vim.g.nvim_tree_highlight_opened_files = 1
            vim.g.nvim_tree_lsp_diagnostics = 1
            vim.g.nvim_tree_update_cwd = 1

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
                    ["core.norg.completion"] = {
                        config = {
                            engine = "nvim-cmp",
                        },
                    },
                    ["core.norg.concealer"] = {
                        config = {
                            icons = {
                                heading = {
                                    enabled = true,
                                    level_1 = {
                                        icon = "♠",
                                    },
                                    level_2 = {
                                        icon = " ♣",
                                    },
                                    level_3 = {
                                        icon = "  ♥",
                                    },
                                    level_4 = {
                                        icon = "   ♦",
                                    },
                                },
                            },
                        },
                    },
                },
                hook = function()
                    vim.opt.shiftwidth = 2
                end
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
                org_indent_mode = "indent",
            }
        end,
    }
    use {
        "akinsho/org-bullets.nvim",
        after = "orgmode.nvim",
        config = function()
            require("org-bullets").setup {
                symbols = { "♠", "♣", "♥", "♦" },
            }
        end,
    }
    -- }}}
    -- }}}

    -- language-specific {{{
    use { "alaviss/nim.nvim", ft = "nim" }
    use "nanotee/nvim-lua-guide"
    use {
        "folke/lua-dev.nvim",
        ft = "lua",
        config = function()
            require("lspconfig").sumneko_lua.setup(require("lua-dev").setup {
                lspconfig = {
                    cmd = { "lua-language-server" },
                },
            })
        end,
    }
    -- }}}

    -- keybindings {{{
    use {
        "folke/which-key.nvim",
        config = function()
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
                        r = { "<cmd>Telescope reloader<cr>", "Reload module"}
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
        end,
    }
    -- }}}
end)

-- }}}
