(import-macros {: load! : trigger! : key! : tx!} :config.macros)
(import-macros {: merge!} :hibiscus.core)

(load! :actions-preview.nvim
       {:keys [(key! :<localleader>a
                     #(let [actions-preview (require :actions-preview)]
                        (actions-preview.code_actions))
                     {:desc "Code actions"})]
        :after #(let [actions-preview (require :actions-preview)]
                  (actions-preview.setup {:backend [:nui]
                                          :nui {:layout {:relative :cursor
                                                         :position 0}
                                                :preview {:border {:style :solid}}
                                                :select {:border {:style :solid}}}}))})

(load! :conform.nvim
       {:keys [(key! :<localleader>f
                     #(let [conform (require :conform)]
                        (conform.format {:lsp_format :fallback}))
                     {:desc :Format :mode [:n :v]})]
        :cmd [:ConformInfo]
        :after #(let [conform (require :conform)]
                  (conform.setup {:formatters_by_ft {:fennel [:fnlfmt]
                                                     :fish [:fish_indent]
                                                     :just [:just]
                                                     :lua [:stylua]
                                                     :sh [:shfmt]}}))})

(load! :fidget.nvim
       {:event [:BufReadPre :BufNewFile]
        :after #(let [fidget (require :fidget)]
                  (fidget.setup {:progress {:display {:done_style :diffAdded
                                                      :progress_style :Comment
                                                      :group_style :FloatTitle
                                                      :icon_style :Normal}}
                                 :notification {:window {:winblend 0
                                                         :normal_hl :NormalFloat
                                                         :relative :win}}}))})

(load! :glance.nvim
       {:cmd :Glance
        :keys [(key! :<localleader>D "<cmd>Glance definitions<cr>"
                     {:desc :Definitions})
               (key! :<localleader>R "<cmd>Glance references<cr>"
                     {:desc :References})
               (key! :<localleader>i "<cmd>Glance implementations<cr>"
                     {:desc :Implementations})]
        :after #(let [glance (require :glance)]
                  (glance.setup {:detached true
                                 :theme {:enable false}
                                 :mappings (let [actions glance.actions]
                                             {:list {:<Tab> (actions.enter_win :preview)
                                                     :<C-j> (actions.preview_scroll_win -5)
                                                     :<C-k> (actions.preview_scroll_win 5)}
                                              :preview {:<Tab> (actions.enter_win :list)}})
                                 :folds {:fold_closed "󰅂"
                                         :fold_open "󰅀"
                                         :folded false}}))})

(load! :lsplinks.nvim {:keys [(key! :gx
                                    #(let [lsplinks (require :lsplinks)]
                                       (lsplinks.gx)))]
                       :after #(let [lsplinks (require :lsplinks)]
                                 (lsplinks.setup))})

(load! :neoconf.nvim
       {:cmd :Neoconf
        :lazy true
        :after #(let [neoconf (require :neoconf)] (neoconf.setup))})

(load! :nvim-lint
       {:event [:BufReadPre :BufNewFile]
        :after #(let [lint (require :lint)]
                  (set lint.linters.zlint
                       {:cmd :zlint
                        :stdin false
                        :args [:-f :gh]
                        :ignore_exitcode true
                        :parser (let [parser (require :lint.parser)]
                                  (parser.from_pattern "^::(%w) file=(%w+),line=(%d+),col=(%d+),title=(%w+)::(%w+)"
                                                       [:severity
                                                        :file
                                                        :line
                                                        :col
                                                        :code
                                                        :message]
                                                       {:error vim.diagnostic.severity.ERROR
                                                        :warning vim.diagnostic.severity.WARN
                                                        :notice vim.diagnostic.severity.INFO}
                                                       {:source :zlint
                                                        :severity vim.diagnostic.severity.WARN}))})
                  (set lint.linters_by_ft
                       {:cmake [:cmakelint]
                        :fish [:fish]
                        :less [:stylelint]
                        :make [:checkmake]
                        :nix [:statix]
                        :zig [:zlint]})
                  (vim.api.nvim_create_autocmd [:BufWritePost
                                                :BufReadPost
                                                :InsertLeave]
                                               {:callback #(lint.try_lint)}))})

(load! :nvim-lspconfig
       {:event [:BufReadPre :BufNewFile]
        :before #(do
                   (trigger! :nvim-navic)
                   (trigger! :blink.cmp))
        :after #(do
                  (trigger! :neoconf.nvim)
                  (vim.lsp.inlay_hint.enable)
                  (vim.lsp.config "*"
                                  {:on_attach (fn [client bufnr]
                                                (when (or (client.supports_method :textDocument/inlayHint)
                                                          client.server_capabilities.inlayHintProvider)
                                                  (vim.lsp.inlay_hint.enable true
                                                                             {: bufnr}))
                                                (let [navic (require :nvim-navic)]
                                                  (when client.server_capabilities.documentSymbolProvider
                                                    (navic.attach client bufnr))))
                                   :capabilities (let [blink (require :blink.cmp)]
                                                   (merge! (vim.lsp.protocol.make_client_capabilities)
                                                           (blink.get_lsp_capabilities {}
                                                                                       false)))})
                  (each [server config (pairs {:basedpyright {}
                                               :bashls {}
                                               :clangd {:capabilities {:offsetEncoding :utf-16}}
                                               :cssls {}
                                               :emmet_language_server {:filetypes [:html
                                                                                   :superhtml]}
                                               :fennel_language_server {:settings {:fennel {:workspace {:library (vim.api.nvim_list_runtime_paths)}
                                                                                            :diagnostics {:globals [:vim
                                                                                                                    :case]}}}}
                                               :harper_ls {:settings {:harper-ls {:linters {:BoringWords true}}}
                                                           :filetypes [:gitcommit
                                                                       :markdown
                                                                       :typst]}
                                               :html {}
                                               :jsonls {:before_init (fn [_
                                                                          config]
                                                                       (trigger! :SchemaStore.nvim)
                                                                       (set config.settings.json
                                                                            {:schemas (let [schemastore (require :schemastore)]
                                                                                        (schemastore.json.schemas))
                                                                             :validate {:enable true}}))}
                                               :lua_ls {}
                                               :marksman {}
                                               :mesonlsp {}
                                               :neocmake {:init_options {:format {:enable true}
                                                                         :lint {:enable true}}}
                                               :nixd {:settings {:nixd {:formatting {:command [:nixfmt]}
                                                                        :options {:nixos {:expr (.. "(builtins.getFlake \"/home/bandithedoge/dotfiles\").nixosConfigurations."
                                                                                                    (vim.fn.hostname)
                                                                                                    :.options)}
                                                                                  :home-manager {:expr "(builtins.getFlake \"/home/bandithedoge/dotfiles\").legacyPackages.${builtins.currentSystem}.homeConfigurations.bandithedoge.options"}
                                                                                  :flake-parts {:expr "(builtins.getFlake \"/home/bandithedoge/dotfiles\").debug.options"}
                                                                                  :flake-parts2 {:expr "(builtins.getFlake \"/home/bandithedoge/dotfiles\").currentSystem.options"}}}}}
                                               :cmd [:nixd
                                                     :--semantic-tokens=true]
                                               :oxlint {}
                                               :ruff {}
                                               :rust_analyzer {}
                                               :stylelint {}
                                               :superhtml {}
                                               :taplo {}
                                               :tinymist {:settings {:formatterMode :typstyle}}
                                               :ts_ls {}
                                               :yamlls {:before_init (fn [_
                                                                          config]
                                                                       (trigger! :SchemaStore.nvim)
                                                                       (set config.settings.yaml
                                                                            {:schemaStore {:enable false
                                                                                           :url ""}
                                                                             :schemas (let [schemastore (require :schemastore)]
                                                                                        (schemastore.yaml.schemas))}))}
                                               :zls {:settings {:zls {:inlay_hints_hide_redundant_param_names true
                                                                      :inlay_hints_hide_redundant_param_names_last_token true
                                                                      :warn_style true
                                                                      :highlight_global_var_declarations true}}}})]
                    (vim.lsp.config server config)
                    (vim.lsp.enable server)))})

(load! :nvim-navic
       {:lazy true
        :after #(let [navic (require :nvim-navic)]
                  (navic.setup {:highlight true :separator " 󰅂 "}))})

(load! :outline.nvim
       {:cmd [:Outline :OutlineOpen]
        :keys [(key! :<localleader>s :<cmd>Outline<cr> {:desc :Symbols})]
        :after #(let [outline (require :outline)]
                  (outline.setup {:outline_window {:width 20
                                                   :hide_cursor true
                                                   :winhl "Normal:NormalNC,CursorLine:CursorLineNC"}
                                  :symbols {:filter (tx! :String
                                                         {:exclude true})
                                            :icon_fetcher #(_G.MiniIcons.get :lsp
                                                                             $1)}}))})
