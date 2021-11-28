-- lualine {{{
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
        lualine_y = { { "bo:shiftwidth" }, "fileformat", "filetype" },
        lualine_z = { "location", "progress" },
    },
}
-- }}}

-- nvim-bufferline.lua {{{
require("bufferline").setup()
-- }}}

-- indent-blankline.nvim {{{
require("indent_blankline").setup {
    show_current_context = true,
    char_list = { "|", "¦", "┆", "┊" },
    use_treesitter = true,
    filetype_exclude = { "help", "NvimTree", "packer", "TelescopePrompt" },
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
    update_cwd = true,
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
        command_history = {
            theme = "ivy",
            border = false,
        },
        reloader = {
            theme = "ivy",
            border = false,
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

fterm_lazygit = fterm:new {
    cmd = "lazygit",
}
-- }}}

-- Shade.nvim {{{
require("shade").setup()
-- }}}
