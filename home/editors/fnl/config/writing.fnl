(let [neorg (require :neorg)]
  (neorg.setup {:load {:core.defaults {}
                       :core.norg.completion {:config {:engine :nvim-cmp}}
                       :core.norg.dirman {:config {:workspaces {:school "~/sql"}
                                                   :autochdir true}}
                       :core.norg.esupports.metagen {:config {:type :auto
                                                              :template [[:title
                                                                          (lambda []
                                                                            (vim.fn.expand "%:p:t:r"))]
                                                                         [:created
                                                                          (lambda []
                                                                            (os.date "%Y-%m-%d"))]]}}
                       :core.norg.concealer {:config {:icons {:heading {:enabled true
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
                                                              :markup {:icon " "}}}}}}))

(let [orgmode (require :orgmode)]
  (orgmode.setup {:org_hide_leading_stars true :org_indent_mode :indent}))
