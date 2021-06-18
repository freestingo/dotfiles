require'nvim-treesitter.configs'.setup {
    ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
    highlight = {
        enable = true, -- false will disable the whole extension
        custom_captures = {
            ["moreKeywords"] = "JavaMoreKeywords",
        },
    },
    query_linter = {
        enable = true,
        use_virtual_text = true,
        lint_events = {"BufWrite", "CursorHold"},
    },
    textobjects = {
        select = {
            enable = true,
            keymaps = {
                ["ac"] = "@call.outer",
                ["ic"] = "@call.inner",
                ["aC"] = "@class.outer",
                ["iC"] = "@class.inner",
                ["aP"] = "@parameter.outer",
                ["iP"] = "@parameter.inner",
            },
        },
        swap = {
            enable = true,
            swap_next = {
                ["<leader>pn"] = "@parameter.inner",
            },
            swap_previous = {
                ["<leader>pp"] = "@parameter.inner",
            },
        },
    },
}
