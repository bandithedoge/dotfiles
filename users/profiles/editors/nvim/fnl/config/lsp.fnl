(require-macros :hibiscus.vim)

(let [lsp (require :lspconfig)
      lua-dev (require :lua-dev)
      cmp_nvim_lsp (require :cmp_nvim_lsp)
      schemastore (require :schemastore)
      closing_labels (require :lsp_extensions.dart.closing_labels)
      yaml (require :yaml-companion)
      defaults {:capabilities (vim.tbl_deep_extend :force
                                                   (cmp_nvim_lsp.update_capabilities (vim.lsp.protocol.make_client_capabilities))
                                                   {:textDocument {:completion {:completionItem {:snippetSupport true}}}})
                :single_file_support true}
      servers {:bashls {}
               :clangd {}
               :cssls {}
               :eslint {:filetypes [:coffee
                                    :javascript
                                    :javascript.jsx
                                    :javascriptreact
                                    :pug
                                    :typescript
                                    :typescript.tsx
                                    :typescriptreact
                                    :vue]
                        :packageManager :pnpm}
               :gdscript {}
               :gopls {}
               :hls {:haskell {:hlintOn true} :formattingProvider :fourmolu}
               :html {}
               :jsonls {:settings {:json {:schemas (schemastore.json.schemas)
                                          :validate {:enable true}}}}
               :nimls {}
               :psalm {}
               :pylsp {}
               :rnix {}
               :rust_analyzer {}
               :solargraph {}
               :sumneko_lua (lua-dev.setup {:runtime_path true})
               :tsserver {}
               :yamlls (yaml.setup)
               :zls {}}]
  (each [server config (pairs servers)]
    (let [s (. lsp server)]
      (s.setup (vim.tbl_deep_extend :force defaults config)))))

(let [null-ls (require :null-ls)
      b null-ls.builtins
      f b.formatting
      d b.diagnostics
      a b.code_actions]
  (null-ls.setup)
  (null-ls.register [f.alejandra
                     f.black
                     f.cabal_fmt
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
                                                           params.options.tabSize]))
                                       :extra_filetypes [:toml :coffee :pug]})
                     d.fish
                     d.shellcheck
                     d.statix
                     d.stylelint
                     (d.markdownlint.with {:command :markdownlint-cli2})
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

(let [lsp-lines (require :lsp_lines)]
  (lsp-lines.register_lsp_virtual_lines)
  (vim.diagnostic.config {:virtual_text false :virtual_lines true}))
