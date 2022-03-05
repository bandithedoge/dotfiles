(let [feline (require :feline)
      vi_mode (require :feline.providers.vi_mode)]
  (feline.setup {:components {:active (let [padding {:provider (lambda []
                                                                 " ")
                                                     :hl (lambda []
                                                           {:name (vi_mode.get_mode_highlight_name)
                                                            :bg (vi_mode.get_mode_color)})}]
                                        [[padding
                                          {:provider {:name :vi_mode
                                                      :opts {:show_mode_name true
                                                             :padding false}}
                                           :icon ""
                                           :hl (lambda []
                                                 {:name (vi_mode.get_mode_highlight_name)
                                                  :bg (vi_mode.get_mode_color)
                                                  :fg base00
                                                  :style :bold})}
                                          padding
                                          {:provider {:name :file_info
                                                      :opts {:file_modified_icon " "
                                                             :file_readonly_icon " "
                                                             :type :unique}}
                                           :icon ""
                                           :left_sep " "
                                           :right_sep "  "}
                                          {:provider :lsp_client_names
                                           :right_sep " "
                                           :hl {:fg base03}}
                                          {:provider :diagnostic_info
                                           :hl {:fg base0D}}
                                          {:provider :diagnostic_hints
                                           :hl {:fg base0B}}
                                          {:provider :diagnostic_warnings
                                           :hl {:fg base09}}
                                          {:provider :diagnostic_errors
                                           :hl {:fg base08}}]
                                         []
                                         [{:provider :git_diff_added
                                           :hl {:fg base0B}}
                                          {:provider :git_diff_changed
                                           :hl {:fg base0E}}
                                          {:provider :git_diff_removed
                                           :hl {:fg base08}}
                                          {:provider :git_branch
                                           :left_sep "  "
                                           :hl {:fg base03}}
                                          {:provider {:name :file_type
                                                      :opts {:case :lowercase
                                                             :filetype_icon true}}
                                           :right_sep " "
                                           :left_sep "  "}
                                          padding
                                          {:provider :position
                                           :hl (lambda []
                                                 {:name (vi_mode.get_mode_highlight_name)
                                                  :bg (vi_mode.get_mode_color)
                                                  :fg base00})}
                                          padding]])
                              :inactive [[{:provider :file_info
                                           :icon ""
                                           :left_sep " "
                                           :hl {:fg base03}}]
                                         []]}
                 :theme {:fg base05 :bg base01}
                 :vi_mode_colors {:NORMAL base0F
                                  :OP base0F
                                  :INSERT base0B
                                  :VISUAL base0A
                                  :LINES base0A
                                  :BLOCK base0A
                                  :REPLACE base08
                                  :V-REPLACE base08
                                  :ENTER base0C
                                  :MORE base0C
                                  :SELECT base0D
                                  :COMMAND base0E
                                  :SHELL base0E
                                  :TERM base0E
                                  :NONE base0F}
                 :force_inactive {:filetypes [:^NvimTree$ :^help$]
                                  :buftypes [:^terminal$]}}))

(let [cokeline (require :cokeline)
      utils (require :cokeline.utils)]
  (cokeline.setup {:default_hl {:focused {:fg (utils.get_hex :Normal :bg)
                                          :bg (utils.get_hex :FloatBorder :fg)}
                                :unfocused {:fg (utils.get_hex :Comment :fg)
                                            :bg (utils.get_hex :StatusLine :bg)}}
                   :components [{:text (lambda [buffer]
                                         (.. " " buffer.devicon.icon))
                                 :hl {}
                                 :truncation {:priority 1}}
                                {:text (lambda [buffer]
                                         buffer.unique_prefix)
                                 :hl {:style :italic}
                                 :truncation {:priority 3 :direction :left}}
                                {:text (lambda [buffer]
                                         (.. buffer.filename " "))}
                                {:text (lambda [buffer]
                                         (or (and (not= buffer.diagnostics.errors
                                                        0)
                                                  (.. " "
                                                      buffer.diagnostics.errors
                                                      " "))
                                             (and (not= buffer.diagnostics.warnings
                                                        0)
                                                  (.. " "
                                                      buffer.diagnostics.warnings
                                                      " "))
                                             ""))}
                                {:text (lambda [buffer]
                                         (or (and buffer.is_modified " ") ""))}]}))
