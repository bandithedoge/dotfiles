(_G.use :nvim-neo-tree/neo-tree.nvim
        {:dependencies [(_G.use :nvim-lua/plenary.nvim)
                        (_G.use :nvim-tree/nvim-web-devicons)
                        (_G.use :MunifTanjim/nui.nvim)]
         :keys [(_G.key :<leader>t "<cmd>Neotree toggle<cr>"
                        {:desc "File tree"})]
         :cmd :Neotree
         :opts {:sources [:filesystem]
                :use_default_mappings false
                :default_component_configs {:container {:enable_character_fade false}
                                            :name {:trailing_slash true
                                                   :use_git_status_colors false}
                                            :git_status {:symbols {:added ""
                                                                   :deleted ""
                                                                   :modified ""
                                                                   :renamed ""
                                                                   :untracked ""
                                                                   :ignored ""
                                                                   :unstaged ""
                                                                   :staged ""
                                                                   :conflict "󰘬"}
                                                         :align :left}
                                            :modified {:symbol "󰆓"}}
                :window {:width 30
                         :mappings {:<2-LeftMouse> :open
                                    :<cr> :open
                                    :s :open_split
                                    :v :open_vsplit
                                    :R :refresh
                                    :a {1 :add
                                        :config {:show_path :relative}}
                                    :d :delete
                                    :r :rename
                                    :y :copy_to_clipboard
                                    :x :cut_to_clipboard
                                    :p :paste_from_clipboard
                                    :q :close_window
                                    :? :show_help}}
                :filesystem {:window {:mappings {:H :toggle_hidden
                                                 :/ :fuzzy_finder
                                                 :h :navigate_up
                                                 :l :set_root}}
                             :filtered_items {:hide_dotfiles false
                                              :hide_gitignored false
                                              :hide_hidden false}}}})
