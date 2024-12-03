(import-macros {: merge!} :hibiscus.core)

[(_G.use :Saghen/blink.cmp
         {:dependencies [(_G.use :saghen/blink.compat)
                         (_G.use :onsails/lspkind.nvim
                                 {:opts {:preset :codicons}})]
          :event :InsertEnter
          :opts #{:keymap {:<C-Space> [:show
                                       :show_documentation
                                       :hide_documentation]
                           :<C-j> [:select_next :fallback]
                           :<C-k> [:select_prev :fallback]
                           :<CR> [:accept :fallback]
                           :<C-h> [:scroll_documentation_up]
                           :<C-l> [:scroll_documentation_down]
                           :<Tab> [:snippet_forward :fallback]
                           :<S-Tab> [:snippet_backward :fallback]}
                  :accept {:expand_snippet #(let [luasnip (require :luasnip)]
                                              (luasnip.lsp_expand $...))
                           :auto_brackets {:enabled true}}
                  :trigger {:signature_help {:enabled true}}
                  :fuzzy {:prebuilt_binaries {:download (not _G.USING_NIX)}}
                  :sources {:completion {:enabled_providers [:lsp
                                                             :lazydev
                                                             :neorg
                                                             :orgmode
                                                             :path
                                                             :snippets]}
                            :providers {:lsp {:fallback_for [:lazydev]}
                                        :lazydev {:name :LazyDev
                                                  :module :lazydev.integrations.blink}
                                        :neorg {:name :neorg
                                                :module :blink.compat.source}
                                        :orgmode {:name :orgmode
                                                  :module :blink.compat.source}}}
                  :nerd_font_variant :normal
                  :windows {:autocomplete {:max_height 15
                                           :min_width 25
                                           :selection :manual
                                           :draw {:columns [(merge! [:kind_icon
                                                                     :label
                                                                     :label_description]
                                                                    {:gap 1})
                                                            [:kind]]}}
                            :documentation {:auto_show true}
                            :signature_help {:border :none}}
                  :highlight {:use_nvim_cmp_as_default true}
                  :kind_icons (let [lspkind (require :lspkind)] lspkind.symbol_map)}})]
