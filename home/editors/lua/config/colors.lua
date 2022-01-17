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
        bg = _G.base00,
        bg_alt = _G.base01,
        bg_popup = _G.base02,
        bg_statusline = _G.base01,
        bg_sidebar = _G.base10,
        bg_float = _G.base02,
        bg_visual = _G.base03,
        bg_search = _G.base0F,
        bg_highlight = _G.base02,

        fg = _G.base05,
        fg_alt = _G.base04,
        fg_gutter = _G.base02,
        fg_sidebar = _G.base04,

        black = _G.base01,
        red = _G.base08,
        green = _G.base0B,
        yellow = _G.base0A,
        blue = _G.base0D,
        magenta = _G.base0E,
        cyan = _G.base0C,
        white = _G.base05,
        orange = _G.base09,
        pink = util.blend(_G.base08, _G.base07, 0.5),

        black_br = _G.base02,
        red_br = _G.base12,
        green_br = _G.base14,
        yellow_br = _G.base13,
        blue_br = _G.base16,
        magenta_br = _G.base17,
        cyan_br = _G.base15,
        white_br = _G.base07,
        orange_br = util.blend(_G.base12, _G.base13, 0.5),
        pink_br = util.blend(_G.base12, _G.base07, 0.5),

        black_dm = _G.base11,
        red_dm = util.darken(_G.base08, 0.3),
        green_dm = util.darken(_G.base0B, 0.3),
        yellow_dm = util.darken(_G.base0A, 0.3),
        blue_dm = util.darken(_G.base0D, 0.3),
        magenta_dm = util.darken(_G.base0E, 0.3),
        cyan_dm = util.darken(_G.base0C, 0.3),
        white_dm = _G.base04,
        orange_dm = util.darken(_G.base09, 0.3),
        pink_dm = util.darken(util.blend(_G.base08, _G.base07, 0.5), 0.3),

        comment = _G.base03,
        variable = _G.base05,

        border = _G.base02,
        border_highlight = _G.base0F,

        error = _G.base08,
        warning = _G.base09,
        info = _G.base0D,
        hint = _G.base0B,

        git = {
            add = _G.base0B,
            change = _G.base09,
            delete = _G.base08,
            conflict = _G.base0E,
            ignore = _G.base03,
        },

        gitSigns = {
            add = _G.base0B,
            change = _G.base09,
            delete = _G.base08,
        },

        diff = {
            add = _G.base0B,
            change = _G.base09,
            delete = _G.base08,
            text = _G.base0D,
        },
    },

    hlgroups = {
        PmenuSel = { fg = "${bg}", bg = "${border_highlight}" },
        NormalPopover = { bg = "${bg_popup}" },
        Folded = { fg = "${fg_alt}", bg = "${bg_alt}" },

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
