require("nvim-treesitter.configs").setup {
    highlight = { enable = true },
    indent = { enable = true },
    autopairs = { enable = true },
    rainbow = { enable = true, extended_mode = true },
    playground = { enable = true },
}

require("nvim-autopairs").setup {
    enable_check_bracket_line = false,
    check_ts = true,
}
