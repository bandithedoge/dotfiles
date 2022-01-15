-- nvim-treesitter {{{
require("nvim-treesitter.configs").setup {
    highlight = { enable = true },
    indent = { enable = true },
    rainbow = { enable = true, extended_mode = true },
    playground = { enable = true },
    autotag = { enable = true },
    context_commentstring = { enable = true },
}
-- }}}

-- spellsitter.nvim {{{
require("spellsitter").setup()
-- }}}
