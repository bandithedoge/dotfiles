(require-macros :hibiscus.vim)
(require-macros :hibiscus.core)

(exec! [sign define DiagnosticSignError text= texthl=DiagnosticSignError]
       [sign define DiagnosticSignWarn text= texthl=DiagnosticSignWarn]
       [sign define DiagnosticSignInfo text= texthl=DiagnosticSignInfo]
       [sign define DiagnosticSignHint text=󰌵 texthl=DiagnosticSignHint])

[(_G.use :neovim/nvim-lspconfig
         {:event [:BufReadPre :BufNewFile]
          :dependencies [(_G.use :williamboman/mason.nvim)
                         (_G.use :folke/neodev.nvim {:config true})
                         (_G.use :akinsho/flutter-tools.nvim
                                 {:opts {:ui {:border [" "
                                                       " "
                                                       " "
                                                       " "
                                                       " "
                                                       " "
                                                       " "
                                                       " "]}}})
                         (_G.use :MrcJkb/haskell-tools.nvim
                                 {:dependencies [(_G.use :nvim-telescope/telescope.nvim)]
                                  :ft [:haskell :lhaskell :cabal :cabalproject]
                                  :init #(set vim.g.haskell_tools
                                              {:tools {:hover {:disable true}}
                                               :hls {:default_settings {:haskell {:formattingProvider :fourmolu}}}})})
                         (_G.use :jose-elias-alvarez/typescript.nvim)
                         (_G.use :vuki656/package-info.nvim
                                 {:opts {:colors {:up_to_date _G.base0B
                                                  :outdated _G.base08}
                                         :package_manager :pnpm}})
                         (_G.use :Saecki/crates.nvim
                                 {:opts {:null_ls {:enabled true}}})
                         (_G.use :hrsh7th/nvim-cmp)
                         (_G.use :b0o/SchemaStore.nvim)
                         (_G.use :mrshmllow/document-color.nvim)
                         (_G.use :someone-stole-my-name/yaml-companion.nvim)
                         (_G.use :p00f/clangd_extensions.nvim)]
          :opts #(let [cmp-nvim-lsp (require :cmp_nvim_lsp)]
                   {:capabilities (merge! (cmp-nvim-lsp.default_capabilities)
                                          {:textDocument {:completion {:completionItem {:snippetSupport true}}}}
                                          :workspace
                                          {:didChangeWatchedFiles {:dynamicRegistration true}})}
                   :single_file_support
                   true)
          :config #(let [lsp (require :lspconfig)]
                     (lsp.bashls.setup $2)
                     (lsp.clangd.setup (merge! $2
                                               {:on_attach #(let [inlay-hints (require :clangd_extensions.inlay_hints)]
                                                              ; (map! [n :buffer] :<localleader>ls :ClangdSwitchSourceHeader "Switch source/header")
                                                              (inlay-hints.setup_autocmd)
                                                              (inlay-hints.set_inlay_hints))}))
                     (lsp.cssls.setup $2)
                     (lsp.dartls.setup $2)
                     (lsp.emmet_language_server.setup $2)
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
                     (lsp.html.setup $2)
                     (lsp.jsonls.setup (merge! $2
                                               (let [schemastore (require :schemastore)]
                                                 {:settings {:json {:schemas (schemastore.json.schemas)
                                                                    :validate {:enable true}}}})))
                     (lsp.lua_ls.setup $2)
                     (lsp.marksman.setup $2)
                     (lsp.neocmake.setup $2)
                     (lsp.nil_ls.setup (merge! $2 {:autostart true}))
                     (lsp.nixd.setup (merge! $2
                                             {:settings {:nixd {:formatting {:command :alejandra}}}}))
                     (lsp.nimls.setup $2)
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
                     (lsp.rust_analyzer.setup $2)
                     (lsp.qmlls.setup $2)
                     (lsp.tailwindcss.setup (merge! $2
                                                    (let [document-color (require :document-color)]
                                                      {:capabilities {:textDocument {:colorProvider {:dynamicRegistration true}}}
                                                       :on_attach document-color.buf_attach})))
                     (lsp.yamlls.setup (let [yaml (require :yaml-companion)]
                                         (yaml.setup $2)))
                     (lsp.zls.setup $2)
                     (let [typescript (require :typescript)]
                       (typescript.setup {:server {:on_attach #(map! [n
                                                                      :buffer]
                                                                     :<localleader>ld
                                                                     :TypescriptGoToSourceDefinition
                                                                     "Go to source definition")}})))})
 ;;
 (_G.use :nvimtools/none-ls.nvim
         {:dependencies [(_G.use :neovim/nvim-lspconfig)
                         (_G.use :jose-elias-alvarez/typescript.nvim)]
          :event [:BufReadPre :BufNewFile]
          :config #(let [null-ls (require :null-ls)
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
                                        f.shellharden
                                        f.shfmt
                                        f.stylelint
                                        f.stylua
                                        f.trim_newlines
                                        f.trim_whitespace
                                        (f.prettierd.with {:extra_args (lambda [params]
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
                                        d.clazy
                                        d.cmake_lint
                                        d.editorconfig_checker
                                        d.fish
                                        d.shellcheck
                                        d.trail_space
                                        a.gitrebase
                                        a.shellcheck
                                        (require :typescript.extensions.null-ls.code-actions)]))})
 ;;
 (_G.use :j-hui/fidget.nvim
         {:dependencies [(_G.use :neovim/nvim-lspconfig)]
          :event [:BufReadPre :BufNewFile]
          :opts {:text {:spinner :dots :done "󰄬"}}})
 ;;
 (_G.use :ray-x/lsp_signature.nvim
         {:dependencies [(_G.use :neovim/nvim-lspconfig)]
          :event [:BufReadPre :BufNewFile]
          :opts {:bind true
                 :handler_opts {:border :solid}
                 :hint_prefix "󰌵"
                 :floating_window false
                 :floating_window_above_cur_line true}})
 ;;
 (_G.use :DNLHC/glance.nvim
         {:dependencies [(_G.use :neovim/nvim-lspconfig)]
          :keys [(_G.key :<localleader> "<cmd>Glance definitions<cr>"
                         {:desc :Definitions})]
          :opts {:border {:enable true :top_char "" :bottom_char ""}}})]
