(require-macros :hibiscus.vim)
(require-macros :hibiscus.core)

(let [lsp (require :lspconfig)
      lua-dev (require :lua-dev)
      cmp_nvim_lsp (require :cmp_nvim_lsp)
      schemastore (require :schemastore)
      yaml (require :yaml-companion)
      defaults {:capabilities (merge (cmp_nvim_lsp.update_capabilities (vim.lsp.protocol.make_client_capabilities))
                                     {:textDocument {:completion {:completionItem {:snippetSupport true}}}})
                :single_file_support true}]
  (lsp.bashls.setup defaults)
  (lsp.clangd.setup defaults)
  (lsp.cssls.setup defaults)
  (lsp.eslint.setup (merge defaults
                           {:filetypes [:coffee
                                        :javascript
                                        :javascript.jsx
                                        :javascriptreact
                                        :pug
                                        :typescript
                                        :typescript.tsx
                                        :typescriptreact
                                        :vue]
                            :settings {:packageManager :pnpm}}))
  (lsp.gdscript.setup defaults)
  (lsp.gopls.setup defaults)
  (lsp.hls.setup (merge defaults
                        {:settings {:haskell {:formattingProvider :fourmolu}}}))
  (lsp.html.setup defaults)
  (lsp.jsonls.setup (merge defaults
                           {:settings {:json {:schemas (schemastore.json.schemas)
                                              :validate {:enable true}}}}))
  (lsp.nimls.setup defaults)
  (lsp.psalm.setup defaults)
  (lsp.pylsp.setup defaults)
  (lsp.rnix.setup defaults)
  (lsp.rust_analyzer.setup defaults)
  (lsp.solargraph.setup defaults)
  (lsp.sumneko_lua.setup (lua-dev.setup {:runtime_path true}))
  (lsp.tailwindcss.setup defaults)
  (lsp.tsserver.setup defaults)
  (lsp.yamlls.setup (yaml.setup))
  (lsp.zls.setup defaults))

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
                     f.stylelint
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
  (lsp-lines.setup)
  (vim.diagnostic.config {:virtual_text false :virtual_lines true}))
