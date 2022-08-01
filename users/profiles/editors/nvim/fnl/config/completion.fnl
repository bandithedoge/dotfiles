(let [cmp (require :cmp)
      luasnip (require :luasnip)
      lspkind (require :lspkind)
      cmp-under-comparator (require :cmp-under-comparator)]
  (cmp.setup {:snippet {:expand (lambda [args]
                                  (luasnip.lsp_expand args.body))}
              :completion {:completeopt "menu,menuone,preview,noinsert,noselect"}
              :preselect cmp.PreselectMode.None
              :mapping {:<C-Space> (cmp.mapping.complete)
                        :<CR> (cmp.mapping.confirm)
                        :<C-j> (cmp.mapping.select_next_item {:behavior cmp.SelectBehavior.Select})
                        :<C-k> (cmp.mapping.select_prev_item {:behavior cmp.SelectBehavior.Select})
                        :<C-h> (cmp.mapping.scroll_docs -4)
                        :<C-l> (cmp.mapping.scroll_docs 4)
                        :<Tab> (cmp.mapping (lambda [fallback]
                                              (if (luasnip.expand_or_jumpable)
                                                  (luasnip.expand_or_jump)
                                                  (fallback))))
                        :<S-Tab> (lambda [fallback]
                                   (if (luasnip.jumpable -1)
                                       (luasnip.jump -1)
                                       (fallback)))}
              :formatting {:format (lspkind.cmp_format {:with_text true})}
              :sources [{:name :crates}
                        {:name :emoji}
                        {:name :luasnip}
                        {:name :neorg}
                        {:name :nvim_lsp}
                        {:name :nvim_lua}
                        {:name :orgmode}
                        {:name :path}
                        {:name :treesitter}]
              :sorting {:comparators [cmp.config.compare.exact
                                      cmp.config.compare.offset
                                      cmp.config.compare.score
                                      cmp-under-comparator.under
                                      cmp.config.compare.kind
                                      cmp.config.compare.sort_text
                                      cmp.config.compare.length
                                      cmp.config.compare.order]}})
  (cmp.setup.cmdline ":" {:sources [{:name :cmdline} {:name :path}]})
  (cmp.setup.cmdline "/" [{:name :buffer} {:name :nvim_lsp_document_symbol}]))
