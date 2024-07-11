[(_G.use :L3MON4D3/LuaSnip
         {:dependencies [(_G.use :rafamadriz/friendly-snippets
                                 {:config #(let [loader (require :luasnip.loaders.from_vscode)]
                                             (loader.lazy_load))})]
          :event :InsertEnter
          :keys [(_G.key :<tab> #(let [luasnip (require :luasnip)]
                                   (if (luasnip.jumpable 1) (luasnip.jump 1)
                                       :<tab>))
                         {:expr true :silent true :mode :i})
                 ; (_G.key :<tab> #(let [luasnip (require :luasnip)]
                 ;                   (luasnip.jump 1)
                 ;                   {:mode :s}))
                 (_G.key :<S-tab>
                         #(let [luasnip (require :luasnip)] (luasnip.jump -1)
                            {:mode [:i :s]}))]
          :config #(let [luasnip (require :luasnip)
                         snip (fn snip [name dscr]
                                "Returns a simple text snippet for LuaSnip"
                                (let [s luasnip.snippet
                                      t luasnip.text_node]
                                  (s {:trig name : name : dscr} (t name))))
                         base16 {:base00 ["Default Background"
                                          "Example: #282c34"]
                                 :base01 ["Lighter Background (Used for status bars, line number and folding marks)"
                                          "Example: #3f4451"]
                                 :base02 ["Selection Background"
                                          "Example: #4f5666"]
                                 :base03 ["Comments, Invisibles, Line Highlighting"
                                          "Example: #545862"]
                                 :base04 ["Dark Foreground (used for status bars)"
                                          "Example: #9196a1"]
                                 :base05 ["Default Foreground, Caret, Delimiters, Operators"
                                          "Example: #abb2bf"]
                                 :base06 ["Light Foreground (Not often used)"
                                          "Example: #e6e6e6"]
                                 :base07 ["Light Background (Not often used)"
                                          "Example: #ffffff"]
                                 :base08 ["Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted"
                                          "Example: #e05561 (Red)"]
                                 :base09 ["Integers, Boolean, Constants, XML Attributes, Markup Link Url"
                                          "Example: #d18f52 (Orange)"]
                                 :base0A ["Classes, Markup Bold, Search Text Background"
                                          "Example: #e6b965 (Yellow)"]
                                 :base0B ["Strings, Inherited Class, Markup Code, Diff Inserted"
                                          "Example: #8cc265 (Green)"]
                                 :base0C ["Support, Regular Expressions, Escape Characters, Markup Quotes"
                                          "Example: #42b3c2 (Cyan)"]
                                 :base0D ["Functions, Methods, Attribute IDs, Headings"
                                          "Example: #4aa5f0 (Blue)"]
                                 :base0E ["Keywords, Storage, Selector, Markup Italic, Diff Changed"
                                          "Example: #c162de (Magenta)"]
                                 :base0F ["Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php>"
                                          "Example: #bf4034"]
                                 :base10 ["Darker Background"
                                          "Example: #21252b"]
                                 :base11 ["Darkest Background"
                                          "Example: #181a1f"]
                                 :base12 ["Bright Red" "Example: #ff616e"]
                                 :base13 ["Bright Yellow" "Example: #f0a45d"]
                                 :base14 ["Bright Green" "Example: #a5e075"]
                                 :base15 ["Bright Cyan" "Example: #4cd1e0"]
                                 :base16 ["Bright Blue" "Example: #4dc4ff"]
                                 :base17 ["Bright Magenta" "Example: #de73ff"]}]
                     (luasnip.add_snippets :all
                                           (icollect [name dscr (pairs base16)]
                                             (snip name dscr))))})]

