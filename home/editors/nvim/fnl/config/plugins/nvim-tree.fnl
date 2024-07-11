(import-macros {: g!} :hibiscus.vim)

[(_G.use :nvim-tree/nvim-tree.lua
         {:dependencies [(_G.use :nvim-tree/nvim-web-devicons)
                         (_G.use :nvim-telescope/telescope.nvim)
                         (_G.use :antosha417/nvim-lsp-file-operations {:config true})]
          :keys [(_G.key :<leader>t :<cmd>NvimTreeToggle<cr>
                         {:desc "File tree"})]
          :init #(do
                   (g! loaded_netrw 1)
                   (g! loaded_netrwPlugin 1))
          :opts {:on_attach #(let [api (require :nvim-tree.api)]
                               (api.config.mappings.default_on_attach $1)
                               (vim.keymap.set :n :h
                                               api.tree.change_root_to_parent
                                               {:buffer $1
                                                :noremap true
                                                :silent true
                                                :nowait true})
                               (vim.keymap.set :n :l
                                               api.tree.change_root_to_node
                                               {:buffer $1
                                                :noremap true
                                                :silent true
                                                :nowait true}))
                 :hijack_cursor true
                 :hijack_netrw true
                 :hijack_unnamed_buffer_when_opening true
                 :sync_root_with_cwd true
                 :reload_on_bufenter true
                 :respect_buf_cwd true
                 :select_prompts true
                 :view {:preserve_window_proportions true}
                 :renderer {:add_trailing true
                            :highlight_git false
                            :highlight_opened_files :icon
                            :highlight_modified :name
                            :indent_markers {:enable true}
                            :root_folder_label false
                            :icons {:web_devicons {:folder {:enable true
                                                            :color false}}
                                    :glyphs {:modified "󰆓"
                                             :git {:unstaged ""
                                                   :staged ""
                                                   :unmerged ""
                                                   :renamed ""
                                                   :untracked ""
                                                   :deleted ""
                                                   :ignored ""}}
                                    :git_placement :signcolumn
                                    :modified_placement :before
                                    :show {:folder_arrow false :modified false}}}
                 :update_focused_file {:enable true}
                 :git {:show_on_open_dirs false}
                 :diagnostics {:enable true
                               :show_on_dirs true
                               :show_on_open_dirs false}
                 :modified {:enable true :show_on_open_dirs false}
                 :filters {:git_ignored false}}})]

