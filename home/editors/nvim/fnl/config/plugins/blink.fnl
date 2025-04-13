(import-macros {: use! : tx!} :config.macros)

[(use! :Saghen/blink.cmp
       {:dependencies [(use! :rafamadriz/friendly-snippets)
                       (use! :xzbdmw/colorful-menu.nvim {:opts {}})]
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
                :completion {:list {:selection {:preselect false
                                                :auto_insert false}}
                             :menu {:min_width 25
                                    :max_height 15
                                    :draw {:treesitter [:lsp :lazydev]
                                           :columns #(if (= $1.mode :cmdline)
                                                         [[:label]]
                                                         [(tx! :kind_icon
                                                               :label {:gap 1})
                                                          [:kind]])
                                           :components {:kind_icon {:ellipsis false
                                                                    :text #(let [icons (require :mini.icons)]
                                                                             (local (icon _
                                                                                          _)
                                                                                    (icons.get :lsp
                                                                                               $1.kind))
                                                                             icon)}
                                                        :label {:text #(let [colorful-menu (require :colorful-menu)]
                                                                         (colorful-menu.blink_components_text $1))
                                                                :highlight #(let [colorful-menu (require :colorful-menu)]
                                                                              (colorful-menu.blink_components_highlight $1))}}}}
                             :documentation {:auto_show true
                                             :auto_show_delay_ms 0}}
                :signature {:enabled true :window {:border :none}}
                :fuzzy {:prebuilt_binaries {:download (not _G.USING_NIX)}
                        :sorts [:exact :score :sort_text]}
                :sources {:default [:lsp :path :snippets]
                          :per_filetype {:lua [:lazydev]}
                          :providers {:lazydev {:name :LazyDev
                                                :module :lazydev.integrations.blink
                                                :score_offset 100
                                                :fallbacks [:lsp]}}}
                :appearance {:use_nvim_cmp_as_default true
                             :nerd_font_variant :normal}}})]
