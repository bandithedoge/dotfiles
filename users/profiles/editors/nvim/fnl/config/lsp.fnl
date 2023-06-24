(require-macros :hibiscus.vim)
(require-macros :hibiscus.core)

(local defaults (let [cmp_nvim_lsp (require :cmp_nvim_lsp)]
                  {:capabilities (merge! (cmp_nvim_lsp.default_capabilities)
                                         {:textDocument {:completion {:completionItem {:snippetSupport true}}}})
                   :single_file_support true}))

;; lua {{{
(let [neodev (require :neodev)]
  (neodev.setup {}))

;; }}}

;; flutter {{{
(let [flutter-tools (require :flutter-tools)]
  (flutter-tools.setup {:ui {:border [" " " " " " " " " " " " " " " "]}}))

;; }}}

;; haskell {{{
(let [haskell-tools (require :haskell-tools)]
  (haskell-tools.setup {:tools {:hover {:border nil}}
                        :hls {:default_settings {:haskell {:formattingProvider :fourmolu}}
                              :on_attach #(do
                                            (map! [n :buffer] :<localleader>lh
                                                  `haskell-tools.hoogle.hoogle_signature
                                                  :Hoogle)
                                            (map! [n :buffer] :<localleader>lr
                                                  `haskell-tools.repl.toggle
                                                  "GHCi repl"))}}))

;; }}}

;; typescript {{{
(let [typescript (require :typescript)]
  (typescript.setup {:server {:on_attach #(map! [n :buffer] :<localleader>ld
                                                :TypescriptGoToSourceDefinition
                                                "Go to source definition")}}))

;; }}}

;; package.json {{{
(let [package-info (require :package-info)]
  (package-info.setup {:colors {:up_to_date _G.base0B :outdated _G.base08}
                       :package_manager :pnpm}))

;; }}}

;; cargo {{{
(let [crates (require :crates)]
  (crates.setup {:null_ls {:enabled true}}))

;; }}}

(exec! [sign define DiagnosticSignError text= texthl=DiagnosticSignError]
       [sign define DiagnosticSignWarn text= texthl=DiagnosticSignWarn]
       [sign define DiagnosticSignInfo text= texthl=DiagnosticSignInfo]
       [sign define DiagnosticSignHint text=󰌵 texthl=DiagnosticSignHint])

(let [lsp (require :lspconfig)]
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
  (lsp.gdscript.setup defaults)
  (lsp.gopls.setup defaults)
  (lsp.html.setup defaults)
  (lsp.jsonls.setup (merge! defaults
                            (let [schemastore (require :schemastore)]
                              {:settings {:json {:schemas (schemastore.json.schemas)
                                                 :validate {:enable true}}}})))
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
  (lsp.tailwindcss.setup (merge! defaults
                                 (let [document-color (require :document-color)]
                                   {:capabilities {:textDocument {:colorProvider {:dynamicRegistration true}}}
                                    :on_attach document-color.buf_attach})))
  (lsp.yamlls.setup (let [yaml (require :yaml-companion)]
                      (yaml.setup {:lspconfig {:on_attach #(map! [n :buffer]
                                                                 :<localleader>ls
                                                                 `yaml.open_ui_select
                                                                 "Select YAML schema")}})))
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
                     a.gitrebase
                     a.shellcheck
                     (require :typescript.extensions.null-ls.code-actions)]))

;; lsp_signature.nvim {{{
(let [lsp_signature (require :lsp_signature)]
  (lsp_signature.setup {:bind true
                        :handler_opts {:border :solid}
                        :hint_prefix "󰌵"
                        :floating_window false
                        :floating_window_above_cur_line true}))

;; }}}

;; lspkind.nvim {{{
(let [lspkind (require :lspkind)]
  (lspkind.init))

;; }}}

;; lsp_extensions.nvim {{{
(let [lsp_extensions (require :lsp_extensions)]
  (lsp_extensions.inlay_hints {:enabled [:TypeHint
                                         :ChainingHint
                                         :ParameterHint]}))

;; }}}

;; lsp_lines.nvim {{{
(let [lsp-lines (require :lsp_lines)]
  (lsp-lines.setup)
  (vim.diagnostic.config {:virtual_text false
                          :virtual_lines {:highlight_whole_line false
                                          :only_current_line true}}))

;; }}}

;; fidget.nvim {{{
(let [fidget (require :fidget)]
  (fidget.setup {:text {:spinner :dots}}))

;; }}}

;; trouble.nvim {{{
(let [trouble (require :trouble)]
  (trouble.setup {:auto_preview false :use_diagnostic_signs true}))

;; }}}

;; glance.nvim {{{
(let [glance (require :glance)]
  (glance.setup {:border {:enable true :top_char "" :bottom_char ""}}))

;; }}}
