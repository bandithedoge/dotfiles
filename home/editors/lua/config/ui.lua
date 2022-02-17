-- lualine {{{
local gps = require "nvim-gps"
gps.setup {
    separator = "  ",
}

require("lualine").setup {
    options = {
        theme = "nightfox",
        component_separators = "",
        section_separators = "",
    },
    extensions = { "nvim-tree" },
    sections = {
        lualine_a = { "mode" },
        lualine_b = { { "filename", path = 0 } },
        lualine_c = {
            { "diagnostics", sources = { "nvim_diagnostic" }, update_in_insert = true },
            { gps.get_location, cond = gps.is_available },
        },
        lualine_x = { "diff" },
        lualine_y = {
            {
                function()
                    return " " .. vim.bo.shiftwidth
                end,
            },
            "fileformat",
            "filetype",
        },
        lualine_z = { "location" },
    },
}
-- }}}

-- indent-blankline.nvim {{{
require("indent_blankline").setup {
    show_current_context = true,
    char = "│",
    use_treesitter = true,
    filetype_exclude = { "help", "TelescopePrompt" },
    buftype_exclude = { "terminal" },
    show_foldtext = false,
}
-- }}}

-- nvim-colorizer.lua {{{
require("colorizer").setup {
    "*",
    DEFAULT_OPTIONS = {
        RRGGBBAA = true,
        css = true,
        css_fn = true,
    },
}
-- }}}

-- gitsigns.nvim {{{
require("gitsigns").setup {
    diff_opts = {
        internal = true,
    },
}
-- }}}

-- nvim-tree.lua {{{
local tree_cb = require("nvim-tree.config").nvim_tree_callback

require("nvim-tree").setup {
    hijack_cursor = true,
    hijack_netrw = true,
    update_cwd = true,
    update_focused_file = {
        enable = true,
    },
    diagnostics = {
        enable = true,
        show_on_dirs = true,
    },
    view = {
        auto_resize = true,
        mappings = {
            list = {
                { key = "h", cb = tree_cb "dir_up" },
                { key = "l", cb = tree_cb "cd" },
            },
        },
    },
    filters = {
        dotfiles = false,
        custom = {
            ".DS_Store",
            ".git",
            "node_modules",
            "__pycache__",
        },
    },
}

vim.g.nvim_tree_indent_markers = 1
vim.g.nvim_tree_show_icons = {
    git = 1,
    folders = 1,
    files = 1,
    folder_arrows = 0,
}
-- }}}

-- telescope.nvim {{{
local telescope = require "telescope"
local actions = require "telescope.actions"

telescope.setup {
    defaults = {
        prompt_prefix = " ",
        border = true,
        borderchars = { " ", " ", " ", " ", " ", " ", " ", " " },
        mappings = {
            i = {
                ["<esc>"] = actions.close,
                ["<C-j>"] = actions.move_selection_next,
                ["<C-k>"] = actions.move_selection_previous,
            },
        },
    },
    pickers = {
        commands = {
            theme = "ivy",
            border = false,
        },
        diagnostics = {
            theme = "ivy",
            border = false,
            preview = false,
        },
        builtin = {
            theme = "ivy",
            border = false,
            preview = false,
        },
        highlights = {
            layout_strategy = "vertical",
        },
    },
    extensions = {
        lsp_handlers = {
            code_action = {
                telescope = require("telescope.themes").get_cursor(),
            },
            definition = {
                telescope = require("telescope.themes").get_cursor(),
            },
            type_definition = {
                telescope = require("telescope.themes").get_cursor(),
            },
            symbol = {
                telescope = require("telescope.themes").get_cursor(),
            },
            implementation = {
                telescope = require("telescope.themes").get_cursor(),
            },
        },
    },
}

telescope.load_extension "dap"
telescope.load_extension "frecency"
-- }}}

-- FTerm.nvim {{{
local fterm = require "FTerm"

fterm_float = fterm:new {
    hl = "NormalPopover",
    border = "solid",
}
-- }}}

-- mini.nvim {{{
require("mini.trailspace").setup()
require("mini.cursorword").setup()
-- }}}

-- fm-nvim {{{
require("fm-nvim").setup {
    ui = {
        float = {
            border = "solid",
            float_hl = "NormalPopover",
        },
    },
}
-- }}}

-- nvim-hlslens {{{
require("hlslens").setup()
-- }}}

-- specs.nvim {{{
local specs = require "specs"
specs.setup {
    popup = {
        winhl = "PmenuSel",
        fader = specs.exp_fader,
    },
}
-- }}}

-- pretty-fold.nvim {{{
require("pretty-fold").setup {
    fill_char = " ",
    process_comment_signs = false,
    sections = {
        left = {
            "content",
        },
        right = {
            "number_of_folded_lines",
        },
    },
}
-- }}}

-- fidget.nvim {{{
require("fidget").setup {
    text = {
        spinner = "dots",
    },
}
-- }}}

-- lsp_lines.nvim {{{
require("lsp_lines").register_lsp_virtual_lines()
vim.diagnostic.config {
    virtual_text = false,
}
-- }}}

-- cheatsheet.nvim {{{
require("cheatsheet").setup {
    telescope_mappings = {
        ["<CR>"] = require("cheatsheet.telescope.actions").select_or_execute,
    },
}
-- }}}

-- dressing.nvim {{{
require("dressing").setup {
    input = {
        border = "solid",
    },
    select = {
        backend = { "telescope", "nui", "builtin" },
        telescope = {
            theme = "cursor",
        },
    },
}
-- }}}

-- nvim-cokeline {{{
local get_hex = require("cokeline.utils").get_hex
require("cokeline").setup {
    default_hl = {
        focused = {
            fg = get_hex("Normal", "bg"),
            bg = get_hex("FloatBorder", "fg"),
        },
        unfocused = {
            fg = get_hex("Comment", "fg"),
            bg = get_hex("StatusLine", "bg"),
        },
    },
    components = {
        {
            text = function(buffer)
                return " " .. buffer.devicon.icon
            end,
            hl = {},
            truncation = { priority = 1 },
        },
        {
            text = function(buffer)
                return buffer.unique_prefix
            end,
            hl = {
                style = "italic",
            },
            truncation = {
                priority = 3,
                direction = "left",
            },
        },
        {
            text = function(buffer)
                return buffer.filename .. " "
            end,
            hl = {
                style = "bold",
            },
            truncation = {
                priority = 2,
                direction = "left",
            },
        },
        {
            text = function(buffer)
                return (buffer.diagnostics.errors ~= 0 and " " .. buffer.diagnostics.errors .. " ")
                    or (buffer.diagnostics.warnings ~= 0 and " " .. buffer.diagnostics.warnings .. " ")
                    or ""
            end,
        },
        {
            text = function(buffer)
                return (buffer.is_modified and " " or "")
            end,
        },
    },
}
-- }}}
