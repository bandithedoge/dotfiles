(module config.lsp)

(let [lsp (require :lspconfig)
      lua-dev (require :lua-dev)
      cmp_nvim_lsp (require :cmp_nvim_lsp)
      schemastore (require :schemastore)
      servers [:bashls
               :clangd
               :cssls
               :gdscript
               :gopls
               :hls
               :html
               :jsonls
               :nimls
               :psalm
               :pylsp
               :rnix
               :rust_analyzer
               :solargraph
               :zls]]
  (lsp.sumneko_lua.setup (lua-dev.setup {:runtime_path true}))
  (each [_ server (ipairs servers)]
    (let [s (. lsp server)]
      (s.setup {:capabilities (cmp_nvim_lsp.update_capabilities (vim.lsp.protocol.make_client_capabilities))
                :settings {:json {:schemas (schemastore.json.schemas)}
                           :Lua {:diagnostics {:globals [:vim]}
                                 :workspace {:library (vim.api.nvim_get_runtime_file ""
                                                                                     true)
                                             :preloadFileSize 500}}}}))))

(let [null-ls (require :null-ls)
      b null-ls.builtins
      f b.formatting
      d b.diagnostics
      a b.code_actions]
  (null-ls.setup)
  (null-ls.register [
                     f.alejandra
                     f.black
                     f.cabal_fmt
                     f.eslint_d
                     f.fish_indent
                     f.fixjson
                     f.fnlfmt
                     f.isort
                     f.nimpretty
                     f.rubocop
                     f.shellharden
                     f.shfmt
                     f.stylua
                     (f.prettier_d_slim.with {:extra_args (lambda [params]
                                                            (and params.options
                                                                 params.options.tabSize
                                                                 [:--tab-width
                                                                  params.options.tabSize]))})
                     d.deadnix
                     d.editorconfig_checker
                     d.eslint_d
                     d.fish
                     d.shellcheck
                     d.statix
                     (d.markdownlint.with {:command :markdownlint-cli2})
                     a.eslint_d
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