(require-macros :hibiscus.vim)
(require-macros :hibiscus.core)

(vim.fn.sign_define [{:name :DiagnosticSignError
                      :text ""
                      :texthl :DiagnosticSignError}
                     {:name :DiagnosticSignWarn
                      :text ""
                      :texthl :DiagnosticSignWarn}
                     {:name :DiagnosticSignInfo
                      :text ""
                      :texthl :DiagnosticSignInfo}
                     {:name :DiagnosticSignHint
                      :text "󰌵"
                      :texthl :DiagnosticSignHint}])

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
                         (_G.use :someone-stole-my-name/yaml-companion.nvim)]
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
                                               {:on_attach #(map! [n :buffer]
                                                                  :<localleader>ls
                                                                  :<cmd>ClangdSwitchSourceHeader<cr>
                                                                  "Switch source/header")}))
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
                     (lsp.nil_ls.setup (merge! $2
                                               {:autostart true
                                                :settings {:nil {:formatting {:command [:alejandra]}
                                                                 :flake {:autoEvalInputs true}}}}))
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
                     (lsp.qmlls.setup $2)
                     (lsp.rust_analyzer.setup $2)
                     (lsp.swift_mesonls.setup $2)
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
                     (null-ls.register [f.black
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
                                        d.fish
                                        d.shellcheck
                                        a.shellcheck
                                        (require :typescript.extensions.null-ls.code-actions)]))})
 ;;
 (_G.use :j-hui/fidget.nvim
         {:event [:LspAttach] :opts {:text {:spinner :dots :done "󰄬"}}})
 ;;
 (_G.use :ray-x/lsp_signature.nvim
         {:event [:LspAttach]
          :opts {:bind true
                 :handler_opts {:border :solid}
                 :hint_prefix "󰌵"
                 :floating_window false
                 :floating_window_above_cur_line true}})
 ;;
 (_G.use :DNLHC/glance.nvim
         {:dependencies [(_G.use :neovim/nvim-lspconfig)]
          :keys [(_G.key :<localleader>D "<cmd>Glance definitions<cr>"
                         {:desc :Definitions})]
          :opts {:border {:enable true :top_char "" :bottom_char ""}}})
 ;;
 (_G.use :nvimdev/lspsaga.nvim
         {:dependencies [(_G.use :nvim-tree/nvim-web-devicons)]
          :event :LspAttach
          :keys [(_G.key :<localleader>a "<cmd>Lspsaga code_action<cr>"
                         {:desc "Code actions"})
                 (_G.key :K "<cmd>Lspsaga hover_doc<cr>")]
          :opts {:ui {:code_action " 󰌵" :border :solid :title false}
                 :code_action {:num_shortcut false :show_server_name true}
                 :lightbulb {:sign false}
                 :symbol_in_winbar {:enable false :show_file false}}})]
