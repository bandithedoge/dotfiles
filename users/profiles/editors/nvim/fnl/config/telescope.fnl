(let [telescope (require :telescope)
      actions (require :telescope.actions)
      themes (require :telescope.themes)]
  (telescope.setup {:defaults {:prompt_prefix " "
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
  (telescope.load_extension :dap)
  (telescope.load_extension :frecency)
  (telescope.load_extension :yaml_schema)
  (telescope.load_extension :yank_history)
  (telescope.load_extension :zf-native))
