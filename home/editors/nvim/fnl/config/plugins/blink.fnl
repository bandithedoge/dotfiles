(import-macros {: load! : trigger! : tx!} :config.macros)

(load! :colorful-menu.nvim
       {:lazy true
        :after #(let [colorful-menu (require :colorful-menu)]
                  (colorful-menu.setup))})

(load! :blink.cmp
       {:event [:InsertEnter :CmdlineEnter]
        :before #(trigger! :colorful-menu.nvim)
        :after #(let [blink (require :blink.cmp)
                      colorful-menu (require :colorful-menu)]
                  (blink.setup {:keymap {:<C-Space> [:show
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
                                             :menu {:min_width 18
                                                    :max_height 15
                                                    :draw {:treesitter [:lsp
                                                                        :lazydev]
                                                           :columns #(if (= $1.mode
                                                                            :cmdline)
                                                                         [[:label]]
                                                                         [(tx! :kind_icon
                                                                               :label
                                                                               {:gap 1})
                                                                          [:kind]])
                                                           :components {:kind_icon {:ellipsis false
                                                                                    :text #(let [icons (require :mini.icons)]
                                                                                             (local (icon _
                                                                                                          _)
                                                                                                    (icons.get :lsp
                                                                                                               $1.kind))
                                                                                             icon)}
                                                                        :label {:text colorful-menu.blink_components_text
                                                                                :highlight colorful-menu.blink_components_highlight}}}}
                                             :documentation {:auto_show true
                                                             :auto_show_delay_ms 0}}
                                :signature {:enabled true
                                            :window {:border :none}}
                                :fuzzy {:sorts [:exact :score :sort_text]}
                                :sources {:default [:lsp :path :snippets]
                                          :per_filetype {:lua [:lazydev]}
                                          :providers {:lazydev {:name :LazyDev
                                                                :module :lazydev.integrations.blink
                                                                :score_offset 100
                                                                :fallbacks [:lsp]}}}
                                :appearance {:use_nvim_cmp_as_default true
                                             :nerd_font_variant :normal}}))})
