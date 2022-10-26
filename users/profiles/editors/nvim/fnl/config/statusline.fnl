(require-macros :hibiscus.core)

(set vim.o.laststatus 3)

(let [galaxyline (require :galaxyline)
      buffer (require :galaxyline.provider_buffer)
      condition (require :galaxyline.condition)
      fileinfo (require :galaxyline.provider_fileinfo)]
  (set galaxyline.short_line_list [:neo-tree :Trouble])
  (set galaxyline.section.left
       [{:ViMode {:provider #(let [mode (vim.fn.mode)]
                               (vim.api.nvim_command (.. "hi GalaxyViMode guibg="
                                                         (. {:n _G.base0F
                                                             :i _G.base0B
                                                             :v _G.base0A
                                                             :V _G.base0A
                                                             "\022" _G.base0A
                                                             :c _G.base0E
                                                             :s _G.base0D
                                                             :S _G.base0D
                                                             "\019" _G.base0D
                                                             :R _G.base08
                                                             :r _G.base08
                                                             :! _G.base0C
                                                             :t _G.base0C}
                                                            mode)))
                               (.. ;; seems like it always ignores the first space
                                   "  "
                                   (. (let [n :NORMAL
                                            v :VISUAL
                                            s :SELECT
                                            i :INSERT
                                            r :REPLACE
                                            c :COMMAND
                                            q "?"
                                            t :TERMINAL]
                                        {: n
                                         :no n
                                         :nov n
                                         :noV n
                                         "no\022" n
                                         :niI n
                                         :niR n
                                         :niV n
                                         :nt n
                                         : v
                                         :vs v
                                         :V v
                                         :Vs v
                                         "\022" v
                                         "\022s" v
                                         : s
                                         :S s
                                         "\019" s
                                         : i
                                         :ic i
                                         :ix i
                                         :R r
                                         :Rc r
                                         :Rx r
                                         :Rv r
                                         :Rvc r
                                         :Rvx r
                                         : c
                                         :cv c
                                         :r q
                                         :rm q
                                         :r? q
                                         :! t
                                         : t})
                                      mode)
                                   " "))
                  :highlight [_G.base00 _G.base0F :bold]
                  :separator " "
                  :separator_highlight [_G.base00 _G.base02]}}
        {:FileIcon {:provider :FileIcon
                    :highlight [fileinfo.get_file_icon_color _G.base02]
                    :condition condition.buffer_not_empty}}
        {:FileName {:provider #(fileinfo.get_current_file_name "" "")
                    :highlight [_G.base05 _G.base02]
                    :separator " "
                    :separator_highlight [_G.base00 _G.base01]
                    :condition condition.buffer_not_empty}}
        {:DiagnosticError {:provider :DiagnosticError
                           :icon " "
                           :highlight [_G.base08 _G.base01]}}
        {:DiagnosticWarn {:provider :DiagnosticWarn
                          :icon " "
                          :highlight [_G.base0A _G.base01]}}
        {:DiagnosticHint {:provider :DiagnosticHint
                          :icon " "
                          :highlight [_G.base0B _G.base01]}}
        {:DiagnosticInfo {:provider :DiagnosticInfo
                          :icon " "
                          :highlight [_G.base0D _G.base01]}}
        {:Lsp {:provider #(let [servers []]
                            (vim.lsp.for_each_buffer_client 0
                                                            #(table.insert servers
                                                                           $1.name))
                            (table.concat servers " "))
               :highlight [_G.base03 _G.base01]
               :condition condition.check_active_lsp}}])
  (set galaxyline.section.right
       [{:DiffAdd {:provider :DiffAdd
                   :icon " "
                   :highlight [_G.base0B _G.base01]}}
        {:DiffModified {:provider :DiffModified
                        :icon " "
                        :highlight [_G.base0A _G.base01]}}
        {:DiffRemove {:provider :DiffRemove
                      :icon " "
                      :highlight [_G.base08 _G.base01]}}
        {:GitBranch {:provider :GitBranch
                     :icon "שׂ "
                     :highlight [_G.base05 _G.base01]
                     :separator " "
                     :separator_highlight [_G.base00 _G.base01]}}
        {:FileEncoding {:provider #(match vim.bo.fileformat
                                     :dos "  "
                                     :unix "  "
                                     :mac "  ")
                        :highlight [_G.base05 _G.base02]
                        :separator " "
                        :separator_highlight [_G.base00 _G.base01]
                        :condition condition.buffer_not_empty}}
        {:FileTypeName {:provider #(.. (string.lower (buffer.get_buffer_filetype))
                                       " ")
                        :highlight [_G.base05 _G.base02]
                        :condition condition.buffer_not_empty}}])
  (set galaxyline.section.short_line_left
       [{:FileName {:provider #(fileinfo.get_current_file_name "" "")
                    :highlight [_G.base00 _G.base0F]}}]))

(let [bufferline (require :bufferline)]
  (bufferline.setup {:animation false
                     :auto_hide true
                     :maximum_padding 2
                     :icon_separator_active ""
                     :icon_separator_inactive ""
                     :icon_close_tab_modified ""}))
