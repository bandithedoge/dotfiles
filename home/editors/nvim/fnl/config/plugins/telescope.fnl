(import-macros {: use! : key!} :config.macros)

[(use! :nvim-telescope/telescope.nvim
       {:dependencies [(use! :natecraddock/telescope-zf-native.nvim)
                       (use! :stevearc/dressing.nvim
                             {:lazy true
                              :opts #(let [themes (require :telescope.themes)]
                                       {:title_pos :center
                                        :input {:enabled false}
                                        :select {:backend [:telescope :builtin]
                                                 :telescope (themes.get_cursor {:border true
                                                                                :borderchars [" "
                                                                                              " "
                                                                                              " "
                                                                                              " "
                                                                                              " "
                                                                                              " "
                                                                                              " "
                                                                                              " "]})}})})]
        :cmd :Telescope
        :keys [(key! :<leader><space> "<cmd>Telescope commands<cr>"
                     {:desc "Enter command"})
               (key! :<leader>ff "<cmd>Telescope find_files<cr>" {:desc :Files})
               (key! :<leader>fF "<cmd>Telescope oldfiles<cr>" {:desc :Recent})
               (key! :<leader>fg "<cmd>Telescope live_grep<cr>"
                     {:desc "Live Grep"})
               (key! :<leader>fh "<cmd>Telescope help_tags<cr>" {:desc :Help})
               (key! :<leader>fH "<cmd>Telescope highlights<cr>"
                     {:desc "Highlight groups"})
               (key! :<leader>fT "<cmd>Telescope builtin<cr>"
                     {:desc :Telescope})
               (key! :<leader>fo "<cmd>Telescope vim_options<cr>"
                     {:desc :Options})
               (key! :<leader>fk "<cmd>Telescope keymaps<cr>"
                     {:desc :Keybindings})
               (key! :<leader>b "<cmd>Telescope buffers<cr>" {:desc :Buffers})]
        :opts #(let [actions (require :telescope.actions)]
                 {:defaults {:prompt_prefix "❯ "
                             :border true
                             :borderchars [" " " " " " " " " " " " " " " "]
                             :path_display [:smart]
                             :dynamic_preview_title true
                             :mappings {:i {:<esc> actions.close
                                            :<C-j> actions.move_selection_next
                                            :<C-k> actions.move_selection_previous
                                            :<C-h> actions.preview_scrolling_up
                                            :<C-l> actions.preview_scrolling_down
                                            :<C-r> actions.delete_buffer}}}
                  :pickers {:commands {:theme :ivy :border false}}})
        :config #(let [telescope (require :telescope)]
                   (telescope.setup $2)
                   (telescope.load_extension :zf-native))})
 ;;
 (use! :ziontee113/icon-picker.nvim
       {:dependencies [(use! :nvim-telescope/telescope.nvim)]
        :cmd [:IconPickerNormal :IconPickerInsert :IconPickerYank]
        :keys [(key! :<leader>i
                     "<cmd>:IconPickerNormal alt_font emoji nerd_font nerd_font_v3 symbols<cr>"
                     {:desc "Insert symbol"})]
        :config true})]
