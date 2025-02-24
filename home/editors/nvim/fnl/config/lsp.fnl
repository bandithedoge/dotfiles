(import-macros {: use! : key! : tx!} :config.macros)
(import-macros {: merge!} :hibiscus.core)

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
                                  :javascript [:prettierd :eslint_d]
                                  :javascriptreact [:prettierd :eslint_d]
                                  :json [:prettierd]
                                  :jsonc [:prettierd]
                                  :just [:just]
                                  :less [:prettierd :stylelint]
                                  :lua [:stylua]
                                  :markdown [:prettierd]
                                  :markdown.mdx [:prettierd]
                                  :nim [:nimpretty]
                                  :scss [:prettierd :stylelint]
                                  :sh [:shfmt]
                                  :typescript [:prettierd :eslint_d]
                                  :typescriptreact [:prettierd :eslint_d]
                                  :vue [:prettierd :eslint_d]
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
 (use! :lewis6991/hover.nvim
       {:keys [(key! :K #(let [hover (require :hover)] (hover.hover)))]
        :opts {:init #(do
                        (require :hover.providers.lsp)
                        (require :hover.providers.dap))
               :preview_opts {:border :solid}
               :title false}})
 ;;
 (use! nil
       {:url "https://git.sr.ht/~whynothugo/lsp_lines.nvim"
        :event :LazyFile
        :config #(let [lsp-lines (require :lsp_lines)]
                   (lsp-lines.setup)
                   (vim.diagnostic.config {:virtual_text false
                                           :virtual_lines {:highlight_whole_line false
                                                           :only_current_line true}}))}
       :/lsp_lines.nvim)
 ;;
 (use! :icholy/lsplinks.nvim {:lazy true :config true})
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
                         :append_fname false
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
        :dependencies [(use! :AstroNvim/AstroLSP
                             {:dependencies [(use! :AstroNvim/astrocore)
                                             (use! :folke/neoconf.nvim
                                                   {:cmd :Neoconf
                                                    :lazy true
                                                    :config true})
                                             (use! :SmiteshP/nvim-navic
                                                   {:opts {:highlight true
                                                           :separator " 󰅂 "}})
                                             (use! :Saghen/blink.cmp)]
                              :opts #{:features {:inlay_hints true}
                                      :formatting {:format_on_save {:enabled false}}
                                      :handlers (tx! #(let [lsp (require :lspconfig)]
                                                        ((. lsp $1 :setup) $2))
                                                     {:clangd #(let [lsp (require :lspconfig)
                                                                     clangd (require :clangd_extensions)]
                                                                 (lsp.clangd.setup $2)
                                                                 (clangd.setup {}))
                                                      :dartls #(let [flutter-tools (require :flutter-tools)]
                                                                 (flutter-tools.setup {:ui {:border :solid}
                                                                                       :lsp {:color {:enabled true
                                                                                                     :virtual_text false
                                                                                                     :background true}}}))
                                                      :hls false
                                                      :jsonls #(let [lsp (require :lspconfig)
                                                                     schemastore (require :schemastore)]
                                                                 (lsp.jsonls.setup (merge! $2
                                                                                           {:settings {:json {:schemas (schemastore.json.schemas)
                                                                                                              :validate {:enable true}}}})))
                                                      :ltex #(let [ltex-extra (require :ltex_extra)]
                                                               (ltex-extra.setup {:load_langs [:en-US
                                                                                               :pl-PL]
                                                                                  :server_opts $2}))
                                                      :yamlls #(let [lsp (require :lspconfig)
                                                                     schemastore (require :schemastore)]
                                                                 (lsp.yamlls.setup (merge! $2
                                                                                           {:settings {:yaml {:schemaStore {:enable false}
                                                                                                              :url ""}
                                                                                                       :schemas (schemastore.yaml.schemas)}})))})
                                      :capabilities (let [blink (require :blink-cmp)]
                                                      (blink.get_lsp_capabilities (vim.lsp.protocol.make_client_capabilities)))
                                      :mappings {:n {:gx {1 #(let [lsplinks (require :lsplinks)]
                                                               (lsplinks.gx))
                                                          :cond :textDocument/documentLink}}}
                                      :file_operations {:operations {:willRename true
                                                                     :didRename true
                                                                     :willCreate true
                                                                     :didCreate true
                                                                     :willDelete true
                                                                     :didDelete true}}
                                      :servers [:basedpyright
                                                :bashls
                                                :blueprint_ls
                                                :clangd
                                                :cssls
                                                :dartls
                                                :emmet_language_server
                                                :fennel_language_server
                                                :gopls
                                                :hls
                                                :html
                                                :jsonls
                                                :julials
                                                :ltex
                                                :lua_ls
                                                :marksman
                                                :mesonlsp
                                                :neocmake
                                                :nim_langserver
                                                :nixd
                                                :oxlint
                                                :ruff
                                                :rust_analyzer
                                                :slint_lsp
                                                :tailwindcss
                                                :taplo
                                                :ts_ls
                                                :yamlls
                                                :zls]
                                      :config {:clangd {:capabilities {:offsetEncoding :utf-16}}
                                               :fennel_language_server {:settings {:fennel {:workspace {:library (vim.api.nvim_list_runtime_paths)}
                                                                                            :diagnostics {:globals [:vim
                                                                                                                    :case]}}}}
                                               :ltex {:settings {:ltex {:additionalRules {:enablePickyRules true
                                                                                          :motherTongue :pl-PL}}}
                                                      :filetypes [:markdown
                                                                  :org
                                                                  :rst
                                                                  :tex
                                                                  :pandoc
                                                                  :rmd
                                                                  :html
                                                                  :xhtml]}
                                               :nil_ls {:settings {:nil {:formatting {:command [:alejandra]}}}}
                                               :nixd {:settings {:nixd {:formatting {:command [:alejandra]}
                                                                        :options {:nixos {:expr (.. "(builtins.getFlake \"/home/bandithedoge/dotfiles\").nixosConfigurations."
                                                                                                    (vim.fn.hostname)
                                                                                                    :.options)}
                                                                                  :home-manager {:expr "(builtins.getFlake \"/home/bandithedoge/dotfiles\").legacyPackages.${builtins.currentSystem}.homeConfigurations.bandithedoge.options"}
                                                                                  :flake-parts {:expr "(builtins.getFlake \"/home/bandithedoge/dotfiles\").debug.options"}
                                                                                  :flake-parts2 {:expr "(builtins.getFlake \"/home/bandithedoge/dotfiles\").currentSystem.options"}}}}
                                                      :cmd [:nixd
                                                            :--semantic-tokens=true]}}
                                      :on_attach #(let [navic (require :nvim-navic)]
                                                    (when ($1.supports_method :textDocument/documentSymbol)
                                                      (navic.attach $1 $2)))}})
                       (use! :williamboman/mason-lspconfig.nvim
                             {:cond (not _G.USING_NIX)
                              :dependencies [(use! :williamboman/mason.nvim)]
                              :opts {:handlers [#(let [astrolsp (require :astrolsp)]
                                                   (astrolsp.lsp_setup $1))]}})]
        :config #(let [astrolsp (require :astrolsp)]
                   (vim.tbl_map astrolsp.lsp_setup astrolsp.config.servers))})
 ;;
 (use! :hedyhli/outline.nvim
       {:cmd [:Outline :OutlineOpen]
        :keys [(key! :<localleader>s :<cmd>Outline<cr> {:desc :Symbols})]
        :opts {:outline_window {:width 20
                                :hide_cursor true
                                :winhl "Normal:NormalNC,CursorLine:CursorLineNC"}
               :symbols {:filter (tx! :String {:exclude true})
                         :icon_fetcher #(_G.MiniIcons.get :lsp $1)}}})]
