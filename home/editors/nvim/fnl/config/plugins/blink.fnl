(import-macros {: use!} :config.macros)
(import-macros {: merge!} :hibiscus.core)

[(use! :Saghen/blink.cmp
       {:dependencies [(use! :saghen/blink.compat)
                       (use! :rafamadriz/friendly-snippets)]
        :event [:InsertEnter :CmdlineEnter]
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
                :completion {:list {:selection :manual}
                             :menu {:min_width 25
                                    :max_height 15
                                    :draw {:treesitter [:lsp :lazydev]
                                           :columns [(merge! [:kind_icon
                                                              :label
                                                              :label_description]
                                                             {:gap 1})
                                                     [:kind]]
                                           :components {:kind_icon {:ellipsis false
                                                                    :text #(let [icons (require :mini.icons)]
                                                                             (local (icon _
                                                                                          _)
                                                                                    (icons.get :lsp
                                                                                               $1.kind))
                                                                             icon)}}}}
                             :documentation {:auto_show true
                                             :auto_show_delay_ms 0}}
                :signature {:enabled true :window {:border :none}}
                :fuzzy {:prebuilt_binaries {:download (not _G.USING_NIX)}}
                :sources {:default [:lsp :path :snippets]
                          :per_filetype {:lua [:lazydev]
                                         :norg [:neorg]
                                         :org [:orgmode]}
                          :providers {:lazydev {:name :LazyDev
                                                :module :lazydev.integrations.blink
                                                :score_offset 100
                                                :fallbacks [:lsp]}
                                      :neorg {:name :neorg
                                              :module :blink.compat.source}
                                      :orgmode {:name :orgmode
                                                :module :blink.compat.source}}}
                :appearance {:use_nvim_cmp_as_default true
                             :nerd_font_variant :normal}}})]
