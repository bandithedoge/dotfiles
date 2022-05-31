(let [lsp (require :lspconfig)
      lua-dev (require :lua-dev)
      cmp_nvim_lsp (require :cmp_nvim_lsp)
      schemastore (require :schemastore)
      closing_labels (require :lsp_extensions.dart.closing_labels)
      yaml (require :yaml-companion)
      servers [:bashls
               :clangd
               :cssls
               :gdscript
               :gopls
               :hls
               :html
               :jsonls
               :nimls
               :psalm
               :pylsp
               :rnix
               :rust_analyzer
               :solargraph
               :zls]]
  (each [_ server (ipairs servers)]
    (let [s (. lsp server)]
      (s.setup {:capabilities (cmp_nvim_lsp.update_capabilities (vim.lsp.protocol.make_client_capabilities))
                :settings {:json {:schemas (schemastore.json.schemas)}
                           :Lua {:diagnostics {:globals [:vim]}
                                 :workspace {:library (vim.api.nvim_get_runtime_file ""
                                                                                     true)
                                             :preloadFileSize 500}}
                           :haskell {:hlintOn true
                                     :formattingProvider :fourmolu}}
                :init_options {:closingLabels true}
                :callbacks {:dart/TextDocument/publishClosingLabels (closing_labels.get_callback {:highlight :Special
                                                                                                  :prefix " >> "})}})))
  (lsp.sumneko_lua.setup (lua-dev.setup {:runtime_path true}))
  (lsp.yamlls.setup (yaml.setup)))

(let [null-ls (require :null-ls)
      b null-ls.builtins
      f b.formatting
      d b.diagnostics
      a b.code_actions]
  (null-ls.setup)
  (null-ls.register [f.alejandra
                     f.black
                     f.cabal_fmt
                     f.eslint
                     f.fish_indent
                     f.fixjson
                     f.fnlfmt
                     f.isort
                     f.nimpretty
                     f.rubocop
                     f.shellharden
                     f.shfmt
                     f.stylua
                     (f.prettier.with {:extra_args (lambda [params]
                                                     (and params.options
                                                          params.options.tabSize
                                                          [:--tab-width
                                                           params.options.tabSize]))})
                     d.eslint
                     d.fish
                     d.shellcheck
                     d.statix
                     (d.markdownlint.with {:command :markdownlint-cli2})
                     a.eslint
                     a.shellcheck
                     a.statix]))

(let [lsp_signature (require :lsp_signature)]
  (lsp_signature.setup {:bind true
                        :handler_opts {:border :solid}
                        :hint_prefix "漣"
                        :floating_window false
                        :floating_window_above_cur_line true}))

(let [lspkind (require :lspkind)]
  (lspkind.init))

(let [lsp_extensions (require :lsp_extensions)]
  (lsp_extensions.inlay_hints {:enabled [:TypeHint
                                         :ChainingHint
                                         :ParameterHint]}))
