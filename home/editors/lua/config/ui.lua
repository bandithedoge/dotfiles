local vim = _G.vim
-- lualine {{{
local gps = require "nvim-gps"
gps.setup()

require("lualine").setup {
    options = {
        theme = "blueballs",
        component_separators = "",
        section_separators = "",
    },
    extensions = {
        "nvim-tree",
        "chadtree",
        {
            sections = {
                lualine_a = { "mode" },
            },
            filetypes = { "Trouble" },
        },
    },
    sections = {
        lualine_a = { "mode" },
        lualine_b = { { "filename", path = 1 } },
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
    tabline = {
        lualine_a = { "buffers" },
        lualine_b = {},
        lualine_c = {},
        lualine_x = { "lsp_progress" },
        lualine_y = { "branch" },
        lualine_z = { { "tabs", mode = 1 } },
    },
}
-- }}}

-- indent-blankline.nvim {{{
require("indent_blankline").setup {
    show_current_context = true,
    char_list = { "|", "¦", "┆", "┊" },
    use_treesitter = true,
    filetype_exclude = { "help", "NvimTree", "CHADTree", "packer", "TelescopePrompt" },
    buftype_exclude = { "terminal" },
    show_foldtext = false,
}
-- }}}

-- nvim-colorizer.lua {{{
require("colorizer").setup()
-- }}}

-- gitsigns.nvim {{{
require("gitsigns").setup()
-- }}}

-- nvim-tree.lua {{{
local tree_cb = require("nvim-tree.config").nvim_tree_callback

require("nvim-tree").setup {
    hijack_cursor = true,
    update_focused_file = {
        enable = false,
    },
    view = {
        auto_resize = false,
        mappings = {
            list = {
                { key = "h", cb = tree_cb "dir_up" },
                { key = "l", cb = tree_cb "cd" },
            },
        },
    },
    filters = {
        custom = {
            ".DS_Store",
        },
    },
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
        symbols = {
            theme = "cursor",
            border = false,
        },
        lsp_code_actions = {
            theme = "cursor",
            border = false,
        },
        lsp_definitions = {
            theme = "cursor",
            border = false,
        },
        lsp_type_definitions = {
            theme = "cursor",
            border = false,
        },
        lsp_implementations = {
            theme = "cursor",
            border = false,
        },
    },
}

telescope.load_extension "fzy_native"
telescope.load_extension "dap"
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

-- nvim-scrollbar {{{
require("scrollbar").setup {
    excluded_filetypes = {
        "NvimTree",
    },
    handlers = {
        search = true,
    },
}
-- }}}
