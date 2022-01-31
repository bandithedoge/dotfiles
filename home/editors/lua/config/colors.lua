---@diagnostic disable: undefined-global
local nf = require "nightfox"
local util = require "nightfox.util"

nf.setup {
    fox = "nightfox",

    styles = {
        comments = "italic",
        strings = "italic",
        functions = "bold",
    },

    inverse = {
        match_paren = true,
    },

    colors = {
        bg = base00,
        bg_alt = base01,
        bg_popup = base02,
        bg_statusline = base01,
        bg_sidebar = base10,
        bg_float = base02,
        bg_visual = base03,
        bg_search = base0F,
        bg_highlight = base02,

        fg = base05,
        fg_alt = base04,
        fg_gutter = base02,
        fg_sidebar = base04,

        black = base01,
        red = base08,
        green = base0B,
        yellow = base0A,
        blue = base0D,
        magenta = base0E,
        cyan = base0C,
        white = base05,
        orange = base09,
        pink = util.blend(base08, base07, 0.5),

        black_br = base02,
        red_br = base12,
        green_br = base14,
        yellow_br = base13,
        blue_br = base16,
        magenta_br = base17,
        cyan_br = base15,
        white_br = base07,
        orange_br = util.blend(base12, base13, 0.5),
        pink_br = util.blend(base12, base07, 0.5),

        black_dm = base11,
        red_dm = util.darken(base08, 0.3),
        green_dm = util.darken(base0B, 0.3),
        yellow_dm = util.darken(base0A, 0.3),
        blue_dm = util.darken(base0D, 0.3),
        magenta_dm = util.darken(base0E, 0.3),
        cyan_dm = util.darken(base0C, 0.3),
        white_dm = base04,
        orange_dm = util.darken(base09, 0.3),
        pink_dm = util.darken(util.blend(base08, base07, 0.5), 0.3),

        comment = base03,
        variable = base05,

        border = base02,
        border_highlight = base0F,

        error = base08,
        warning = base09,
        info = base0D,
        hint = base0B,

        git = {
            add = base0B,
            change = base09,
            delete = base08,
            conflict = base0E,
            ignore = base03,
        },

        gitSigns = {
            add = base0B,
            change = base09,
            delete = base08,
        },

        diff = {
            add = base0B,
            change = base09,
            delete = base08,
            text = base0D,
        },
    },

    hlgroups = {
        PmenuSel = { fg = "${bg}", bg = "${border_highlight}" },
        Cursor = { fg = "${bg}", bg = "${border_highlight}" },
        NormalPopover = { bg = "${bg_popup}" },
        FloatBorder = { bg = "${bg_popup}", fg = "${border_highlight}" },
        Folded = { fg = "${comment}", bg = "${bg_alt}" },
        LineNr = { fg = "${comment}" },
        StatusLineNC = { fg = "${comment}" },

        TelescopeSelectionCaret = { fg = "${border_highlight}" },
        TelescopeSelection = { fg = "${harsh}", bg = "${bg}" },
        TelescopeNormal = { bg = "${bg_popup}" },
        TelescopeBorder = { bg = "${bg_popup}" },

        IndentBlankline = { fg = "${fg_gutter}" },
        IndentBlanklineChar = { fg = "${fg_gutter}" },
        IndentBlanklineContextChar = { fg = "${border_highlight}" },

        rainbowcol1 = { fg = "${magenta_br}" },
        rainbowcol2 = { fg = "${blue_br}" },
        rainbowcol3 = { fg = "${cyan_br}" },
        rainbowcol4 = { fg = "${green_br}" },
        rainbowcol5 = { fg = "${yellow_br}" },
        rainbowcol6 = { fg = "${orange_br}" },
        rainbowcol7 = { fg = "${red_br}" },
        rainbowcol8 = { fg = "${pink}" },
    },
}

nf._colorscheme_load()
