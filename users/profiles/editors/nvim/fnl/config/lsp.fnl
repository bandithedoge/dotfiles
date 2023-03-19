(require-macros :hibiscus.vim)
(require-macros :hibiscus.core)

(let [neodev (require :neodev)]
  (neodev.setup {}))

(let [flutter-tools (require :flutter-tools)]
  (flutter-tools.setup {:ui {:border [" " " " " " " " " " " " " " " "]}}))

(let [lsp (require :lspconfig)
      cmp_nvim_lsp (require :cmp_nvim_lsp)
      schemastore (require :schemastore)
      yaml (require :yaml-companion)
      document-color (require :document-color)
      defaults {:capabilities (merge! (cmp_nvim_lsp.default_capabilities)
                                      {:textDocument {:completion {:completionItem {:snippetSupport true}}}})
                :single_file_support true}]
  (lsp.bashls.setup defaults)
  (lsp.cmake.setup defaults)
  (lsp.clangd.setup defaults)
  (lsp.cssls.setup defaults)
  (lsp.dartls.setup defaults)
  (lsp.dhall_lsp_server.setup defaults)
  (lsp.emmet_ls.setup defaults)
  (lsp.eslint.setup (merge! defaults
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
  (lsp.fennel_language_server.setup defaults)
  (lsp.gdscript.setup defaults)
  (lsp.gopls.setup defaults)
  (lsp.hls.setup (merge! defaults
                         {:settings {:haskell {:formattingProvider :fourmolu}}
                          :filetypes [:haskell :lhaskell :cabal]}))
  (lsp.html.setup defaults)
  (lsp.jsonls.setup (merge! defaults
                            {:settings {:json {:schemas (schemastore.json.schemas)
                                               :validate {:enable true}}}}))
  (lsp.marksman.setup defaults)
  (lsp.nil_ls.setup (merge! defaults {:autostart true}))
  (lsp.nimls.setup defaults)
  (lsp.lua_ls.setup defaults)
  (lsp.psalm.setup defaults)
  (lsp.purescriptls.setup defaults)
  (lsp.pylsp.setup (merge! defaults
                           {:settings {:pylsp {:plugins {:autopep8 {:enabled false}
                                                         :black {:enabled true
                                                                 :line_length 120}
                                                         :flake8 {:enabled true
                                                                  :maxLineLength 120}
                                                         :pycodestyle {:maxLineLength 120}
                                                         :jedi_completion {:fuzzy true
                                                                           :eager true}
                                                         :pydocstyle {:enabled true
                                                                      :convention :pep257}
                                                         :pylint {:enabled true}
                                                         :yapf {:enabled false}}}}}))
  (lsp.rust_analyzer.setup defaults)
  (lsp.solargraph.setup defaults)
  (lsp.tailwindcss.setup defaults)
  (lsp.tsserver.setup (merge! defaults
                              {:capabilities {:textDocument {:colorProvider {:dynamicRegistration true}}}
                               :on_attach document-color.buf_attach}))
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
                     (d.markdownlint.with {:command :markdownlint-cli2})
                     d.actionlint
                     d.checkmake
                     d.fish
                     d.shellcheck
                     d.statix
                     a.gitrebase
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
