(import-macros {: use! : key!} :config.macros)

[(use! :echasnovski/mini.nvim
       {:lazy false
        :keys [(key! :<leader>w
                     #(let [bufremove (require :mini.bufremove)]
                        (bufremove.delete))
                     {:desc "Close buffer"})
               (key! :<leader><C-w>
                     #(let [bufremove (require :mini.bufremove)]
                        (bufremove.delete 0 true))
                     {:desc "Close buffer (force)"})]
        :init #(set package.preload.nvim-web-devicons
                    #(let [icons (require :mini.icons)]
                       (icons.mock_nvim_web_devicons)
                       package.loaded.nvim-web-devicons))
        :config #(let [bufremove (require :mini.bufremove)
                       icons (require :mini.icons)
                       surround (require :mini.surround)
                       trailspace (require :mini.trailspace)]
                   (bufremove.setup {})
                   (icons.setup {})
                   (surround.setup {:mappings {:add :gsa
                                               :delete :gsd
                                               :find :gsf
                                               :find_left :gsF
                                               :highlight :gsh
                                               :replace :gsr
                                               :update_n_lines :gsn}})
                   (trailspace.setup {}))})]
