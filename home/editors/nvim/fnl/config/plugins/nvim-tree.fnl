(require-macros :hibiscus.vim)

[(_G.use :nvim-tree/nvim-tree.lua
         {:dependencies [(_G.use :nvim-tree/nvim-web-devicons)]
          :keys [(_G.key :<leader>t :<cmd>NvimTreeToggle<cr>
                         {:desc "File tree"})]
          :init #(do
                   (g! loaded_netrw 1)
                   (g! loaded_netrwPlugin 1))
          :opts {:hijack_cursor true
                 :hijack_unnamed_buffer_when_opening true
                 :sync_root_with_cwd true
                 :reload_on_bufenter true
                 :respect_buf_cwd true
                 :select_prompts true
                 :view {:preserve_window_proportions true}
                 :renderer {:add_trailing true
                            :highlight_git false
                            :highlight_opened_files :icon
                            :highlight_modified :icon
                            :indent_markers {:enable true}
                            :icons {:web_devicons {:folder {:enable true}}}}
                 :update_focused_file {:enable true}
                 :diagnostics {:enable true :show_on_dirs true}
                 :modified {:enable true}}})]
