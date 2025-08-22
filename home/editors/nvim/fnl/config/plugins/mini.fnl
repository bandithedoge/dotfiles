(import-macros {: map!} :hibiscus.vim)

(set package.preload.nvim-web-devicons
     #(let [icons (require :mini.icons)]
        (icons.mock_nvim_web_devicons package.loaded.nvim-web-devicons)))

(let [bufremove (require :mini.bufremove)
      icons (require :mini.icons)
      surround (require :mini.surround)
      trailspace (require :mini.trailspace)]
  (bufremove.setup {})
  (map! [:n] :<leader>w bufremove.delete "Close buffer")
  (map! [:n] :<leader><C-w> #(bufremove.delete 0 true) "Close buffer (force)")
  (icons.setup {})
  (surround.setup {:mappings {:add :gsa
                              :delete :gsd
                              :find :gsf
                              :find_left :gsF
                              :highlight :gsh
                              :replace :gsr
                              :update_n_lines :gsn}})
  (trailspace.setup {}))
