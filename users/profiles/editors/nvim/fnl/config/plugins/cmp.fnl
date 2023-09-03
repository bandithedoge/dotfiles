[(_G.use :hrsh7th/nvim-cmp
         {:dependencies [(_G.use :hrsh7th/cmp-cmdline)
                         (_G.use :hrsh7th/cmp-nvim-lsp)
                         (_G.use :hrsh7th/cmp-nvim-lsp-document-symbol)
                         (_G.use :hrsh7th/cmp-nvim-lua)
                         (_G.use :hrsh7th/cmp-path)
                         (_G.use :saadparwaiz1/cmp_luasnip
                                 {:dependencies [(_G.use :L3MON4D3/LuaSnip)]})
                         (_G.use :onsails/lspkind.nvim)
                         (_G.use :lukas-reineke/cmp-under-comparator)
                         (_G.use :p00f/clangd_extensions.nvim)]
          :event [:InsertEnter :CmdlineEnter]
          :opts #(let [cmp (require :cmp)
                       luasnip (require :luasnip)
                       lspkind (require :lspkind)
                       cmp-under-comparator (require :cmp-under-comparator)]
                   {:snippet {:expand (lambda [args]
                                        (luasnip.lsp_expand args.body))}
                    :completion {:completeopt "menu,menuone,preview,noinsert,noselect"}
                    :preselect cmp.PreselectMode.None
                    :mapping {:<C-Space> (cmp.mapping.complete)
                              :<CR> (cmp.mapping.confirm)
                              :<C-j> (cmp.mapping.select_next_item {:behavior cmp.SelectBehavior.Select})
                              :<C-k> (cmp.mapping.select_prev_item {:behavior cmp.SelectBehavior.Select})
                              :<C-h> (cmp.mapping.scroll_docs -4)
                              :<C-l> (cmp.mapping.scroll_docs 4)
                              :<M-Tab> (cmp.mapping #(luasnip.expand_or_jump))
                              :<M-S-Tab> #(luasnip.jump -1)}
                    :formatting {:format (lspkind.cmp_format {:mode :symbol_text})}
                    :experimental {:ghost_text {:hl_group :Comment}}
                    :sources [{:name :crates}
                              {:name :luasnip}
                              {:name :neorg}
                              {:name :nvim_lsp}
                              {:name :nvim_lua}
                              {:name :path}]
                    :sorting {:comparators [cmp.config.compare.exact
                                            cmp.config.compare.offset
                                            cmp.config.compare.score
                                            cmp-under-comparator.under
                                            (require :clangd_extensions.cmp_scores)
                                            cmp.config.compare.kind
                                            cmp.config.compare.sort_text
                                            cmp.config.compare.length
                                            cmp.config.compare.order]}})
          :config #(let [cmp (require :cmp)]
                     (cmp.setup $2)
                     (cmp.setup.cmdline ":"
                                        {:sources [{:name :cmdline}
                                                   {:name :path}]})
                     (cmp.setup.cmdline "/"
                                        [{:name :buffer}
                                         {:name :nvim_lsp_document_symbol}]))})]
