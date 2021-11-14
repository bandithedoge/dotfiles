-- neorg {{{
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
}
-- }}}

-- orgmode.nvim {{{
require("orgmode").setup {
    org_hide_leading_stars = true,
    org_indent_mode = "indent",
}
-- }}}