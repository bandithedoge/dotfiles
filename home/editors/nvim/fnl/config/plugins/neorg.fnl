(import-macros {: use!} :config.macros)

[(use! :nvim-neorg/neorg
       {:dependencies [(use! :nvim-lua/plenary.nvim)
                       (use! :nvim-neorg/lua-utils.nvim)
                       (use! :nvim-neotest/nvim-nio)
                       (use! :MunifTanjim/nui.nvim)
                       (use! :pysan3/pathlib.nvim)]
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
                      :core.export {}
                      :core.concealer {:config {:icons {:heading {:icons ["❋"
                                                                          "❂"
                                                                          "❀"
                                                                          "✿"
                                                                          "❖"
                                                                          "✣"]
                                                                  :highlights [:rainbowcol1
                                                                               :rainbowcol2
                                                                               :rainbowcol3
                                                                               :rainbowcol4
                                                                               :rainbowcol5
                                                                               :rainbowcol6]}}}}}}})]
