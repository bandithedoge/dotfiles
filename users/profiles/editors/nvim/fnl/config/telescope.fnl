[(_G.use :nvim-telescope/telescope.nvim
         {:dependencies [(_G.use :nvim-telescope/telescope-frecency.nvim
                                 {:dependencies [(_G.use :kkharji/sqlite.lua)]})
                         (_G.use :natecraddock/telescope-zf-native.nvim)
                         (_G.use :gbprod/yanky.nvim)
                         (_G.use :someone-stole-my-name/yaml-companion.nvim)
                         (_G.use :stevearc/dressing.nvim
                                 {:dependencies [(_G.use :MunifTanjim/nui.nvim)]
                                  :opts #(let [themes (require :telescope.themes)]
                                           {:title_pos :center
                                            :input {:border :solid
                                                    :win_options {:winblend 0}}
                                            :select {:backend [:telescope
                                                               :nui
                                                               :builtin]
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
          :keys [(_G.key :<leader><space> "<cmd>Telescope commands<cr>"
                         {:desc "Enter command"})
                 (_G.key :<leader>ff "<cmd>Telescope frecency<cr>"
                         {:desc :Files})
                 (_G.key :<leader>fh "<cmd>Telescope help_tags<cr>"
                         {:desc :Help})
                 (_G.key :<leader>fH "<cmd>Telescope highlights<cr>"
                         {:desc "Highlight groups"})
                 (_G.key :<leader>ft "<cmd>Telescope builtin<cr>"
                         {:desc :Telescope})
                 (_G.key :<leader>fg "<cmd>Telescope live_grep<cr>"
                         {:desc "Live Grep"})
                 (_G.key :<leader>b "<cmd>Telescope buffers<cr>"
                         {:desc :Buffers})
                 (_G.key :<leader>h "<cmd>Telescope yank_history<cr>"
                         {:desc "Yank history"})]
          :opts #(let [actions (require :telescope.actions)]
                   {:defaults {:prompt_prefix " "
                               :border true
                               :borderchars [" " " " " " " " " " " " " " " "]
                               :path_display [:smart]
                               :dynamic_preview_title true
                               :mappings {:i {:<esc> actions.close
                                              :<C-j> actions.move_selection_next
                                              :<C-k> actions.move_selection_previous
                                              :<C-h> actions.preview_scrolling_up
                                              :<C-l> actions.preview_scrolling_down}}}
                    :pickers {:commands {:theme :ivy :border false}}})
          :config #(let [telescope (require :telescope)]
                     (telescope.setup $2)
                     (telescope.load_extension :frecency)
                     (telescope.load_extension :yaml_schema)
                     (telescope.load_extension :yank_history)
                     (telescope.load_extension :zf-native))})]
