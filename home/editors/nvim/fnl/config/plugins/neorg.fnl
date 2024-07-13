[(_G.use :nvim-neorg/neorg
         {:dependencies [(_G.use :nvim-lua/plenary.nvim)
                         (_G.use :nvim-neorg/lua-utils.nvim)
                         (_G.use :nvim-neotest/nvim-nio)
                         (_G.use :MunifTanjim/nui.nvim)
                         (_G.use :pysan3/pathlib.nvim)]
          :ft :norg
          :opts {:load {:core.defaults {}
                        :core.esupports.metagen {:config {:type :auto
                                                          :template [[:title
                                                                      #(vim.fn.expand "%:p:t:r")]
                                                                     [:created
                                                                      #(os.date "%Y-%m-%d")]]}}
                        :core.integrations.treesitter {:config {:configure_parsers (not _G.USING_NIX)
                                                                :install_parsers (not _G.USING_NIX)}}
                        :core.keybinds {:config {:neorg_leader :<localleader>l}}
                        :core.completion {:config {:engine :nvim-cmp}}
                        :core.dirman {:config {:workspaces {:school "~/sql"}
                                               :autochdir true}}
                        :core.concealer {:config {:icons {:heading {:enabled true
                                                                    :level_1 {:icon "❋"
                                                                              :highlight :rainbowcol1}
                                                                    :level_2 {:icon " ❂"
                                                                              :highlight :rainbowcol2}
                                                                    :level_3 {:icon "  ❀"
                                                                              :highlight :rainbowcol3}
                                                                    :level_4 {:icon "   ✿"
                                                                              :highlight :rainbowcol4}
                                                                    :level_5 {:icon "    ❖"
                                                                              :highlight :rainbowcol5}
                                                                    :level_6 {:icon "     ✣"
                                                                              :highlight :rainbowcol6}}
                                                          :markup {:icon " "}}}}
                        :core.export {}}}})]

