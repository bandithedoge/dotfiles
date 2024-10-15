(import-macros {: merge!} :hibiscus.core)

[(_G.use :aznhe21/actions-preview.nvim
         {:dependencies [(_G.use :MunifTanjim/nui.nvim)]
          :keys [(_G.key :<localleader>a
                         #(let [actions-preview (require :actions-preview)]
                            (actions-preview.code_actions))
                         {:desc "Code actions"})]
          :opts {:backend [:nui]
                 :nui {:layout {:relative :cursor :position 0}
                       :preview {:border {:style :solid}}
                       :select {:border {:style :solid}}}}})
 (_G.use :stevearc/conform.nvim
         {:keys [(_G.key :<localleader>f
                         #(let [conform (require :conform)]
                            (conform.format {:lsp_format :fallback}))
                         {:desc :Format :mode [:n :v]})]
          :cmd [:ConformInfo]
          :opts {:formatters_by_ft {:css [:prettierd :stylelint]
                                    :fennel [:fnlfmt]
                                    :fish [:fish_indent]
                                    :graphql [:prettierd]
                                    :handlebars [:prettierd]
                                    :html [:prettierd]
                                    :javascript [:prettierd :eslint_d]
                                    :javascriptreact [:prettierd :eslint_d]
                                    :json [:prettierd]
                                    :jsonc [:prettierd]
                                    :less [:prettierd :stylelint]
                                    :lua [:stylua]
                                    :markdown [:prettierd]
                                    :markdown.mdx [:prettierd]
                                    :nim [:nimpretty]
                                    :scss [:prettierd :stylelint]
                                    :sh [:shfmt]
                                    :typescript [:prettierd :eslint_d]
                                    :typescriptreact [:prettierd :eslint_d]
                                    :vue [:prettierd :eslint_d]
                                    :yaml [:prettierd]}}})
 ;;
 (_G.use :j-hui/fidget.nvim
         {:event :LazyFile
          :opts {:progress {:suppress_on_insert true
                            :ignore_done_already true
                            :display {:done_style :diffAdded
                                      :progress_style :Comment
                                      :group_style :FloatTitle
                                      :icon_style :Normal}}}})
 ;;
 (_G.use :DNLHC/glance.nvim
         {:dependencies [(_G.use :neovim/nvim-lspconfig)]
          :keys [(_G.key :<localleader>D "<cmd>Glance definitions<cr>"
                         {:desc :Definitions})]
          :opts {:border {:enable true :top_char "" :bottom_char ""}}})
 ;;
 (_G.use :lewis6991/hover.nvim
         {:keys [(_G.key :K #(let [hover (require :hover)] (hover.hover)))]
          :opts {:init #(do
                          (require :hover.providers.lsp)
                          (require :hover.providers.dap))
                 :preview_opts {:border :solid}
                 :title false}})
 ;;
 (_G.use nil
         {:url "https://git.sr.ht/~whynothugo/lsp_lines.nvim"
          :event :LazyFile
          :config #(let [lsp-lines (require :lsp_lines)]
                     (lsp-lines.setup)
                     (vim.diagnostic.config {:virtual_text false
                                             :virtual_lines {:highlight_whole_line false
                                                             :only_current_line true}}))}
         :/lsp_lines.nvim)
 ;;
 (_G.use :ray-x/lsp_signature.nvim
         {:event [:LspAttach]
          :opts {:bind true
                 :handler_opts {:border :solid}
                 :hint_prefix "󰌵"
                 :floating_window false
                 :floating_window_above_cur_line true}})
 ;;
 (_G.use :icholy/lsplinks.nvim {:lazy true :config true})
 ;;
 (_G.use :mfussenegger/nvim-lint
         {:event :LazyFile
          :opts {:css [:stylelint]
                 :fish [:fish]
                 :less [:stylelint]
                 :make [:checkmake]
                 :nix [:statix :nix]
                 :scss [:stylelint]}
          :config #(let [lint (require :lint)]
                     (set lint.linters_by_ft $2)
                     (vim.api.nvim_create_autocmd [:BufWritePost
                                                   :BufReadPost
                                                   :InsertLeave]
                                                  {:callback #(lint.try_lint)}))})
 ;;
 (_G.use :neovim/nvim-lspconfig
         {:event :LazyFile
          :dependencies [(_G.use :AstroNvim/AstroLSP
                                 {:dependencies [(_G.use :AstroNvim/astrocore)
                                                 (_G.use :folke/neoconf.nvim
                                                         {:cmd :Neoconf
                                                          :lazy true
                                                          :config true})
                                                 (_G.use :SmiteshP/nvim-navic
                                                         {:opts {:highlight true
                                                                 :separator " 󰅂 "}})]
                                  :opts {:features {:autoformat false
                                                    :inlay_hints true}
                                         :formatting {:format_on_save {:enabled false}}
                                         :handlers {1 #(let [lsp (require :lspconfig)]
                                                         ((. lsp $1 :setup) $2))
                                                    :clangd #(let [lsp (require :lspconfig)
                                                                   clangd (require :clangd_extensions)]
                                                               (lsp.clangd.setup $2)
                                                               (clangd.setup {}))
                                                    :dartls #(let [flutter-tools (require :flutter-tools)]
                                                               (flutter-tools.setup {:ui {:border :solid}
                                                                                     :lsp {:color {:enabled true
                                                                                                   :virtual_text false
                                                                                                   :background true}}}))
                                                    :hls false
                                                    :jsonls #(let [lsp (require :lspconfig)
                                                                   schemastore (require :schemastore)]
                                                               (lsp.jsonls.setup (merge! $2
                                                                                         {:settings {:json {:schemas (schemastore.json.schemas)
                                                                                                            :validate {:enable true}}}})))
                                                    :lua_ls #(let [lazydev (require :lazydev)]
                                                               (lazydev.setup))
                                                    :yamlls #(let [lsp (require :lspconfig)
                                                                   schemastore (require :schemastore)]
                                                               (lsp.yamlls.setup (merge! $2
                                                                                         {:settings {:yaml {:schemaStore {:enable false
                                                                                                                          :url ""}
                                                                                                            :schemas (schemastore.yaml.schemas)}}})))}
                                         :capabilities (merge! (vim.lsp.protocol.make_client_capabilities)
                                                               {:textDocument {:completion {:completionItem {:snippetSupport true}}}})
                                         :mappings {:n {:gx {1 #(let [lsplinks (require :lsplinks)]
                                                                  (lsplinks.gx))
                                                             :cond :textDocument/documentLink}}}
                                         :servers [:basedpyright
                                                   :bashls
                                                   :clangd
                                                   :cssls
                                                   :dartls
                                                   :emmet_language_server
                                                   :eslint
                                                   :fennel_language_server
                                                   :gopls
                                                   :hls
                                                   :html
                                                   :jsonls
                                                   :julials
                                                   :ltex
                                                   :lua_ls
                                                   :marksman
                                                   :mesonlsp
                                                   :neocmake
                                                   :nil_ls
                                                   :nim_langserver
                                                   :ruff
                                                   :rust_analyzer
                                                   :tailwindcss
                                                   :taplo
                                                   :ts_ls
                                                   :yamlls
                                                   :zls]
                                         :config {:clangd {:capabilities {:offsetEncoding :utf-16}}
                                                  :eslint {:settings {:packageManager :pnpm}}
                                                  :fennel_language_server {:settings {:fennel {:workspace {:library (vim.api.nvim_list_runtime_paths)}
                                                                                               :diagnostics {:globals [:vim
                                                                                                                       :case]}}}}
                                                  :ltex {:settings {:ltex {:completionEnabled true
                                                                           :additionalRules {:enablePickyRules true
                                                                                             :motherTongue :pl-PL}}}}
                                                  :nil_ls {:settings {:nil {:formatting {:command [:alejandra]}}}}}
                                         :on_attach #(let [navic (require :nvim-navic)]
                                                       (when ($1.supports_method :textDocument/documentSymbol)
                                                         (navic.attach $1 $2)))}})
                         (_G.use :williamboman/mason-lspconfig.nvim
                                 {:cond (not _G.USING_NIX)
                                  :dependencies [(_G.use :williamboman/mason.nvim)]
                                  :opts {:handlers [#(let [astrolsp (require :astrolsp)]
                                                       (astrolsp.lsp_setup $1))]}})]
          :config #(let [astrolsp (require :astrolsp)]
                     (vim.tbl_map astrolsp.lsp_setup astrolsp.config.servers))})]

