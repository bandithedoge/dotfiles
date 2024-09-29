[(_G.use :hrsh7th/nvim-cmp
         {:dependencies [(_G.use :hrsh7th/cmp-cmdline)
                         (_G.use :hrsh7th/cmp-nvim-lsp)
                         (_G.use :hrsh7th/cmp-path)
                         (_G.use :petertriho/cmp-git {:config true})
                         (_G.use :lukas-reineke/cmp-under-comparator)
                         (_G.use :garymjr/nvim-snippets
                                 {:dependencies [(_G.use :rafamadriz/friendly-snippets)]
                                  :opts {:friendly_snippets true}})
                         (_G.use :onsails/lspkind.nvim
                                 {:opts {:mode :symbol_text :preset :codicons}
                                  :config #(let [lspkind (require :lspkind)]
                                             (lspkind.init $1))})]
          :event [:InsertEnter :CmdlineEnter]
          :keys [(_G.key :<Tab> #(if (vim.snippet.active {:direction 1})
                                     "<cmd>lua vim.snippet.jump(1)<cr>"
                                     :<Tab>)
                         {:expr true :silent true :mode [:i :s]})
                 (_G.key :<S-Tab>
                         #(if (vim.snippet.active {:direction -1})
                              "<cmd>lua vim.snippet.jump(-1)<cr>"
                              :<S-Tab>)
                         {:expr true :silent true :mode [:i :s]})]
          :opts #(let [cmp (require :cmp)
                       lspkind (require :lspkind)
                       cmp-under-comparator (require :cmp-under-comparator)
                       clangd (require :clangd_extensions.cmp_scores)]
                   {:completion {:completeopt "menu,menuone,preview,noinsert,noselect"}
                    :preselect cmp.PreselectMode.None
                    :mapping {:<C-Space> (cmp.mapping.complete)
                              :<CR> (cmp.mapping.confirm)
                              :<C-j> (cmp.mapping.select_next_item {:behavior cmp.SelectBehavior.Select})
                              :<C-k> (cmp.mapping.select_prev_item {:behavior cmp.SelectBehavior.Select})
                              :<C-h> (cmp.mapping.scroll_docs -4)
                              :<C-l> (cmp.mapping.scroll_docs 4)}
                    :formatting {:format (lspkind.cmp_format)}
                    :sources [{:name :neorg}
                              {:name :orgmode}
                              {:name :lazydev :group_index 0}
                              {:name :nvim_lsp}
                              {:name :snippets}
                              {:name :git}
                              {:name :path}]
                    :sorting {:comparators [cmp.config.compare.exact
                                            cmp.config.compare.offset
                                            cmp.config.compare.score
                                            cmp-under-comparator.under
                                            clangd
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

