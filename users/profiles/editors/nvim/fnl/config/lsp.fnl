(require-macros :hibiscus.vim)
(require-macros :hibiscus.core)

(exec! [sign define DiagnosticSignError text= texthl=DiagnosticSignError]
       [sign define DiagnosticSignWarn text= texthl=DiagnosticSignWarn]
       [sign define DiagnosticSignInfo text= texthl=DiagnosticSignInfo]
       [sign define DiagnosticSignHint text=󰌵 texthl=DiagnosticSignHint])

[(_G.use :neovim/nvim-lspconfig
         {:event [:BufReadPre :BufNewFile]
          :dependencies [(_G.use :folke/neodev.nvim nil :/lua-dev.nvim)
                         (_G.use :akinsho/flutter-tools.nvim)
                         (_G.use :MrcJkb/haskell-tools.nvim
                                 {:dependencies [(_G.use :nvim-telescope/telescope.nvim)]})
                         (_G.use :jose-elias-alvarez/typescript.nvim)
                         (_G.use :vuki656/package-info.nvim)
                         (_G.use :Saecki/crates.nvim)
                         (_G.use :hrsh7th/nvim-cmp)
                         (_G.use :b0o/SchemaStore.nvim)
                         (_G.use :mrshmllow/document-color.nvim)
                         (_G.use :someone-stole-my-name/yaml-companion.nvim)
                         (_G.use :jose-elias-alvarez/null-ls.nvim)
                         (_G.use :ray-x/lsp_signature.nvim)
                         (_G.use :onsails/lspkind.nvim)
                         (_G.use :nvim-lua/lsp_extensions.nvim)
                         (_G.use nil
                                 {:url "https://git.sr.ht/~whynothugo/lsp_lines.nvim"}
                                 :/lsp_lines.nvim)
                         (_G.use :j-hui/fidget.nvim)
                         (_G.use :folke/trouble.nvim
                                 {:cmd :TroubleToggle
                                  :keys [(_G.key :<localleader>t
                                                 :<cmd>TroubleToggle<cr>
                                                 {:desc :Trouble})]})
                         (_G.use :DNLHC/glance.nvim)]
          :opts #(let [cmp-nvim-lsp (require :cmp_nvim_lsp)]
                   {:capabilities (merge! (cmp-nvim-lsp.default_capabilities)
                                          {:textDocument {:completion {:completionItem {:snippetSupport true}}}
                                           :workspace {:didChangeWatchedFiles {:dynamicRegistration true}}})
                    :single_file_support true})
          :config #(let [lsp (require :lspconfig)]
                     ;; lua {{{
                     (let [neodev (require :neodev)]
                       (neodev.setup)
                       (lsp.lua_ls.setup $2))
                     ;; }}}
                     ;; dart {{{
                     (let [flutter-tools (require :flutter-tools)]
                       (flutter-tools.setup {:ui {:border [" "
                                                           " "
                                                           " "
                                                           " "
                                                           " "
                                                           " "
                                                           " "
                                                           " "]}})
                       (lsp.dartls.setup $2))
                     ;; }}}
                     ;; haskell {{{
                     (let [haskell-tools (require :haskell-tools)]
                       (haskell-tools.setup {:tools {:hover {:border nil}}
                                             :hls {:default_settings {:haskell {:formattingProvider :fourmolu}}
                                                   :on_attach #(do
                                                                 (map! [n
                                                                        :buffer]
                                                                       :<localleader>lh
                                                                       `haskell-tools.hoogle.hoogle_signature
                                                                       :Hoogle)
                                                                 (map! [n
                                                                        :buffer]
                                                                       :<localleader>lr
                                                                       `haskell-tools.repl.toggle
                                                                       "GHCi repl"))}}))
                     ;; }}}
                     ;; web {{{
                     (let [typescript (require :typescript)
                           package-info (require :package-info)]
                       (typescript.setup {:server {:on_attach #(map! [n
                                                                      :buffer]
                                                                     :<localleader>ld
                                                                     :TypescriptGoToSourceDefinition
                                                                     "Go to source definition")}})
                       (package-info.setup {:colors {:up_to_date _G.base0B
                                                     :outdated _G.base08}
                                            :package_manager :pnpm})
                       (lsp.eslint.setup (merge! $2
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
                       (lsp.cssls.setup $2)
                       (lsp.emmet_language_server.setup $2)
                       (lsp.html.setup $2)
                       (lsp.tailwindcss.setup (merge! $2
                                                      (let [document-color (require :document-color)]
                                                        {:capabilities {:textDocument {:colorProvider {:dynamicRegistration true}}}
                                                         :on_attach document-color.buf_attach}))))
                     ;; rust {{{
                     (let [crates (require :crates)]
                       (crates.setup {:null_ls {:enabled true}})
                       (lsp.rust_analyzer.setup $2))
                     ;; }}}
                     (lsp.bashls.setup $2)
                     (lsp.clangd.setup $2)
                     (lsp.dhall_lsp_server.setup $2)
                     (lsp.gdscript.setup $2)
                     (lsp.gopls.setup $2)
                     (lsp.jsonls.setup (merge! $2
                                               (let [schemastore (require :schemastore)]
                                                 {:settings {:json {:schemas (schemastore.json.schemas)
                                                                    :validate {:enable true}}}})))
                     (lsp.marksman.setup $2)
                     (lsp.neocmake.setup $2)
                     (lsp.nil_ls.setup (merge! $2 {:autostart true}))
                     (lsp.nixd.setup (merge! $2 
                                             {:settings {:nixd {:formatting {:command :alejandra}}}}))
                     (lsp.nimls.setup $2)
                     (lsp.psalm.setup $2)
                     (lsp.purescriptls.setup $2)
                     (lsp.pylsp.setup (merge! $2
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
                     (lsp.qmlls.setup $2)
                     (lsp.solargraph.setup $2)
                     (lsp.yamlls.setup (let [yaml (require :yaml-companion)]
                                         (yaml.setup $2)))
                     (lsp.zls.setup $2)
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
                                                            :extra_filetypes [:toml
                                                                              :coffee
                                                                              :pug]})
                                          (d.markdownlint.with {:command :markdownlint-cli2})
                                          d.actionlint
                                          d.checkmake
                                          d.fish
                                          d.shellcheck
                                          a.gitrebase
                                          a.shellcheck
                                          (require :typescript.extensions.null-ls.code-actions)])
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
                         (trouble.setup {:auto_preview false
                                         :use_diagnostic_signs true}))
                       ;; }}}
                       ;; glance.nvim {{{
                       (let [glance (require :glance)]
                         (glance.setup {:border {:enable true
                                                 :top_char ""
                                                 :bottom_char ""}}))))})]
