(module config.telescope)

(let [telescope (require :telescope)
      actions (require :telescope.actions)
      themes (require :telescope.themes)]
  (telescope.setup {:defaults {:prompt_prefix " "
                               :border true
                               :borderchars [" " " " " " " " " " " " " " " "]
                               :mappings {:i {:<esc> actions.close
                                              :<C-j> actions.move_selection_next
                                              :<C-k> actions.move_selection_previous}}}
                    :pickers {:commands {:theme :ivy :border false}
                              :diagnostics {:theme :ivy
                                            :border false
                                            :preview false}
                              :builtin {:theme :ivy
                                        :border false
                                        :preview false}
                              :highlights {:layout_strategy :vertical}}
                    :extensions {:lsp_handlers {:code_action {:telescope (themes.get_cursor)}
                                                :definition {:telescope (themes.get_cursor)}
                                                :type_definition {:telescope (themes.get_cursor)}
                                                :symbol {:telescope (themes.get_cursor)}
                                                :implementation {:telescope (themes.get_cursor)}}}})
  (telescope.load_extension :dap)
  (telescope.load_extension :frecency))
