(import-macros {: use! : key! : tx!} :config.macros)
(import-macros {: merge!} :hibiscus.core)
(import-macros {: augroup!} :hibiscus.vim)

[(use! :aznhe21/actions-preview.nvim
       {:dependencies [(use! :MunifTanjim/nui.nvim)]
        :keys [(key! :<localleader>a
                     #(let [actions-preview (require :actions-preview)]
                        (actions-preview.code_actions))
                     {:desc "Code actions"})]
        :opts {:backend [:nui]
               :nui {:layout {:relative :cursor :position 0}
                     :preview {:border {:style :solid}}
                     :select {:border {:style :solid}}}}})
 ;;
 (use! :stevearc/conform.nvim
       {:keys [(key! :<localleader>f
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
                                  :javascript [:prettierd]
                                  :javascriptreact [:prettierd]
                                  :json [:prettierd]
                                  :jsonc [:prettierd]
                                  :just [:just]
                                  :less [:prettierd :stylelint]
                                  :lua [:stylua]
                                  :markdown [:prettierd]
                                  :markdown.mdx [:prettierd]
                                  :scss [:prettierd :stylelint]
                                  :sh [:shfmt]
                                  :typescript [:prettierd]
                                  :typescriptreact [:prettierd]
                                  :vue [:prettierd]
                                  :yaml [:prettierd]}}})
 ;;
 (use! :j-hui/fidget.nvim
       {:event :LazyFile
        :opts {:progress {:display {:done_style :diffAdded
                                    :progress_style :Comment
                                    :group_style :FloatTitle
                                    :icon_style :Normal}}
               :notification {:window {:winblend 0
                                       :normal_hl :NormalFloat
                                       :relative :win}}}})
 ;;
 (use! :Zeioth/garbage-day.nvim {:event :LspAttach :config true})
 ;;
 (use! :DNLHC/glance.nvim
       {:dependencies [(use! :neovim/nvim-lspconfig)]
        :cmd :Glance
        :keys [(key! :<localleader>D "<cmd>Glance definitions<cr>"
                     {:desc :Definitions})
               (key! :<localleader>R "<cmd>Glance references<cr>"
                     {:desc :References})
               (key! :<localleader>i "<cmd>Glance implementations<cr>"
                     {:desc :Implementations})]
        :opts #(let [glance (require :glance)]
                 {:detached true
                  :theme {:enable false}
                  :mappings (let [actions glance.actions]
                              {:list {:<Tab> (actions.enter_win :preview)
                                      :<C-j> (actions.preview_scroll_win -5)
                                      :<C-k> (actions.preview_scroll_win 5)}
                               :preview {:<Tab> (actions.enter_win :list)}})
                  :folds {:fold_closed "󰅂" :fold_open "󰅀" :folded false}})})
 ;;
 (use! :icholy/lsplinks.nvim {:keys [(key! :gx
                                           #(let [lsplinks (require :lsplinks)]
                                              (lsplinks.gx)))]
                              :config true})
 ;;
 (use! :mfussenegger/nvim-lint
       {:event :LazyFile
        :opts {:cmake [:cmakelint]
               :css [:stylelint]
               :fish [:fish]
               :less [:stylelint]
               :make [:checkmake]
               :nix [:statix]
               :scss [:stylelint]
               :zig [:zlint]}
        :config #(let [lint (require :lint)]
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
                   (set lint.linters_by_ft $2)
                   (vim.api.nvim_create_autocmd [:BufWritePost
                                                 :BufReadPost
                                                 :InsertLeave]
                                                {:callback #(lint.try_lint)}))})
 ;;
 (use! :neovim/nvim-lspconfig
       {:event :LazyFile
        :dependencies [(use! :folke/neoconf.nvim
                             {:cmd :Neoconf :lazy true :config true})
                       (use! :SmiteshP/nvim-navic
                             {:opts {:highlight true} :separator " 󰅂 "})
                       (use! :b0o/SchemaStore.nvim)
                       (use! :Saghen/blink.cmp)
                       (use! :williamboman/mason-lspconfig.nvim
                             {:cond (not _G.USING_NIX)
                              :dependencies [(use! :williamboman/mason.nvim)]})]
        :opts #{:config {:on_attach (fn [client bufnr]
                                      (when (or (client.supports_method :textDocument/inlayHint)
                                                client.server_capabilities.inlayHintProvider)
                                        (vim.lsp.inlay_hint.enable true
                                                                   {: bufnr}))
                                      (let [navic (require :nvim-navic)]
                                        (when client.server_capabilities.documentSymbolProvider
                                          (navic.attach client bufnr))))
                         :capabilities (let [blink (require :blink-cmp)]
                                         (merge! (vim.lsp.protocol.make_client_capabilities)
                                                 (blink.get_lsp_capabilities {}
                                                                             false)))}
                :servers {:basedpyright {}
                          :bashls {}
                          :clangd {:capabilities {:offsetEncoding :utf-16}}
                          :cssls {}
                          :emmet_language_server {}
                          :fennel_language_server {:settings {:fennel {:workspace {:library (vim.api.nvim_list_runtime_paths)}
                                                                       :diagnostics {:globals [:vim
                                                                                               :case]}}}}
                          :harper_ls {:settings {:harper-ls {:linters {:BoringWords true}}}
                                      :filetypes [:gitcommit :markdown :typst]}
                          :html {}
                          :jdtls {}
                          :jsonls {:settings {:json {:schemas (let [schemastore (require :schemastore)]
                                                                (schemastore.json.schemas))
                                                     :validate {:enable true}}}}
                          :ltex {:settings {:ltex {:additionalRules {:enablePickyRules true
                                                                     :motherTongue :pl-PL}}}}
                          :filetypes [:html
                                      :markdown
                                      :org
                                      :pandoc
                                      :rmd
                                      :rst
                                      :tex
                                      :typst
                                      :xhtml]
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
                          :cmd [:nixd :--semantic-tokens=true]
                          :oxlint {}
                          :ruff {}
                          :rust_analyzer {}
                          :taplo {}
                          :tinymist {:settings {:formatterMode :typstyle}}
                          :ts_ls {}
                          :yamlls {:settings {:yaml {:schemaStore {:enable false
                                                                   :url ""}
                                                     :schemas (let [schemastore (require :schemastore)]
                                                                (schemastore.yaml.schemas))}}}
                          :zls {}}}
        :config (fn [_ opts]
                  (vim.lsp.inlay_hint.enable)
                  (vim.lsp.config "*" opts.config)
                  (each [server config (pairs opts.servers)]
                    (vim.lsp.config server config)
                    (vim.lsp.enable server)))})
 ;;
 (use! :hedyhli/outline.nvim
       {:cmd [:Outline :OutlineOpen]
        :keys [(key! :<localleader>s :<cmd>Outline<cr> {:desc :Symbols})]
        :opts {:outline_window {:width 20
                                :hide_cursor true
                                :winhl "Normal:NormalNC,CursorLine:CursorLineNC"}
               :symbols {:filter (tx! :String {:exclude true})
                         :icon_fetcher #(_G.MiniIcons.get :lsp $1)}}})]
