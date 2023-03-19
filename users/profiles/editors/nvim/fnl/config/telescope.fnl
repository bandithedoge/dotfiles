(let [telescope (require :telescope)
      actions (require :telescope.actions)
      themes (require :telescope.themes)]
  (telescope.setup {:defaults {:prompt_prefix " "
                               :border true
                               :borderchars [" " " " " " " " " " " " " " " "]
                               :mappings {:i {:<esc> actions.close
                                              :<C-j> actions.move_selection_next
                                              :<C-k> actions.move_selection_previous
                                              :<C-h> actions.preview_scrolling_up
                                              :<C-l> actions.preview_scrolling_down}}}
                    :pickers {:commands {:theme :ivy :border false}
                              :diagnostics {:theme :ivy
                                            :border false
                                            :preview false}
                              :builtin {:theme :ivy
                                        :border false
                                        :preview false}
                              :highlights {:layout_strategy :vertical}}})
  (telescope.load_extension :dap)
  (telescope.load_extension :frecency)
  (telescope.load_extension :zf-native)
  (telescope.load_extension :yaml_schema))
