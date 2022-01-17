local ls = require "luasnip"
local s = ls.snippet
local t = ls.text_node

ls.snippets = {
    all = {
        s({
            trig = "base00",
            name = "base00",
            dscr = { "Default Background", "Example: #282c34" },
        }, t "base00"),
        s({
            trig = "base01",
            name = "base01",
            dscr = { "Lighter Background (Used for status bars, line number and folding marks)", "Example: #3f4451" },
        }, t "base01"),
        s({
            trig = "base02",
            name = "base02",
            dscr = { "Selection Background", "Example: #4f5666" },
        }, t "base02"),
        s({
            trig = "base03",
            name = "base03",
            dscr = { "Comments, Invisibles, Line Highlighting", "Example: #545862" },
        }, t "base03"),
        s({
            trig = "base04",
            name = "base04",
            dscr = { "Dark Foreground (Used for status bars)", "Example: #9196a1" },
        }, t "base04"),
        s({
            trig = "base05",
            name = "base05",
            dscr = { "Default Foreground, Caret, Delimiters, Operators", "Example: #abb2bf" },
        }, t "base05"),
        s({
            trig = "base06",
            name = "base06",
            dscr = { "Light Foreground (Not often used)", "Example: #e6e6e6" },
        }, t "base06"),
        s({
            trig = "base07",
            name = "base07",
            dscr = { "Light Background (Not often used)", "Example: #ffffff" },
        }, t "base07"),
        s({
            trig = "base08",
            name = "base08",
            dscr = { "Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted", "Example: #e05561 (Red)" },
        }, t "base08"),
        s({
            trig = "base09",

            name = "base09",
            dscr = { "Integers, Boolean, Constants, XML Attributes, Markup Link Url", "Example: #d18f52 (Orange)" },
        }, t "base09"),
        s({
            trig = "base0A",
            name = "base0A",
            dscr = { "Classes, Markup Bold, Search Text Background", "Example: #e6b965 (Yellow)" },
        }, t "base0A"),
        s({
            trig = "base0B",
            name = "base0B",
            dscr = { "Strings, Inherited Class, Markup Code, Diff Inserted", "Example: #8cc265 (Green)" },
        }, t "base0B"),
        s({
            trig = "base0C",
            name = "base0C",
            dscr = { "Support, Regular Expressions, Escape Characters, Markup Quotes", "Example: #42b3c2 (Cyan)" },
        }, t "base0C"),
        s({
            trig = "base0D",
            name = "base0D",
            dscr = { "Functions, Methods, Attribute IDs, Headings", "Example: #4aa5f0 (Blue)" },
        }, t "base0D"),
        s({
            trig = "base0E",
            name = "base0E",
            dscr = { "Keywords, Storage, Selector, Markup Italic, Diff Changed", "Example: #c162de (Purple)" },
        }, t "base0E"),
        s({
            trig = "base0F",
            name = "base0F",
            dscr = { "Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php ?>", "Example: #bf4034" },
        }, t "base0F"),
        s({ trig = "base10", name = "base10", dscr = { "Darker Background", "Example: #21252b" } }, t "base10"),
        s({ trig = "base11", name = "base11", dscr = { "Even Darker Background", "Example: #181a1f" } }, t "base11"),
        s({ trig = "base12", name = "base12", dscr = { "Bright Red", "Example: #ff616e" } }, t "base12"),
        s({ trig = "base13", name = "base13", dscr = { "Bright Yellow", "Example: #f0a45d" } }, t "base13"),
        s({ trig = "base14", name = "base14", dscr = { "Bright Green", "Example: #a5e075" } }, t "base14"),
        s({ trig = "base15", name = "base15", dscr = { "Bright Cyan", "Example: #4cd1e0" } }, t "base15"),
        s({ trig = "base16", name = "base16", dscr = { "Bright Blue", "Example: #4dc4ff" } }, t "base16"),
        s({ trig = "base17", name = "base17", dscr = { "Bright Purple", "Example: #de73ff" } }, t "base17"),
    },
}
