(let [lualine (require :lualine)]
  (lualine.setup {:options {:theme (let [defaults {:a {:normal {:bg _G.base0F
                                                                :fg _G.base00
                                                                :gui :bold}
                                                       :insert {:bg _G.base0B
                                                                :fg _G.base00
                                                                :gui :bold}
                                                       :visual {:bg _G.base0A
                                                                :fg _G.base00
                                                                :gui :bold}
                                                       :replace {:bg _G.base08
                                                                 :fg _G.base00
                                                                 :gui :bold}
                                                       :command {:bg _G.base0E
                                                                 :fg _G.base00
                                                                 :gui :bold}}
                                                   :b {:bg _G.base02
                                                       :fg _G.base04}
                                                   :c {:bg _G.base01
                                                       :fg _G.base04}
                                                   :inactive {:bg _G.base00
                                                              :fg _G.base03}}]
                                     {:normal {:a defaults.a.normal
                                               :b defaults.b
                                               :c defaults.c}
                                      :insert {:a defaults.a.insert
                                               :b defaults.b
                                               :c defaults.c}
                                      :visual {:a defaults.a.visual
                                               :b defaults.b
                                               :c defaults.c}
                                      :replace {:a defaults.a.replace
                                                :b defaults.b
                                                :c defaults.c}
                                      :command {:a defaults.a.command
                                                :b defaults.b
                                                :c defaults.c}
                                      :inactive {:a defaults.inactive
                                                 :b defaults.inactive
                                                 :c defaults.inactive}})
                            :component_separators ""
                            :section_separators ""
                            :globalstatus true}
                  :extensions [:nvim-tree :neo-tree]
                  :sections {:lualine_a [:mode]
                             :lualine_b [{1 :filename :path 0}]
                             :lualine_c [{1 :diagnostics
                                          :sources [:nvim_diagnostic]
                                          :update_in_insert true}]
                             :lualine_x [:diff :branch]
                             :lualine_y [[#(.. " " vim.bo.shiftwidth)]
                                         :fileformat
                                         :filetype]
                             :lualine_z [:location]}}))

(let [bufferline (require :bufferline)]
  (bufferline.setup {:options {:diagnostics :nvim_lsp
                               :separator_style :thick
                               :show_close_icon false
                               :indicator_icon ""}}))
