-- formatter.nvim {{{
local prettier = function()
    return {
        exe = "prettier",
        stdin = true,
    }
end

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
        nix = {
            function()
                return {
                    exe = "nixfmt",
                    stdin = true,
                }
            end,
        },
        html = { prettier },
        javascript = { prettier },
        json = { prettier },
        css = { prettier },
        markdown = { prettier },
        yaml = { prettier },
        toml = { prettier },
    },
}
-- }}}

-- presence.nvim {{{
require("presence"):setup {
    main_image = "file",
}
-- }}}
