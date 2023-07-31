[(_G.use "udayvir-singh/hibiscus.nvim")
 ;;
 (_G.use "rktjmp/hotpot.nvim")
 ;;
 (_G.use "EdenEast/nightfox.nvim")
 ;;
 (_G.use "williamboman/mason.nvim" {:dependencies [(_G.use "williamboman/mason-lspconfig.nvim")
                                                   (_G.use "jay-babu/mason-null-ls.nvim" {:dependencies [(_G.use "jose-elias-alvarez/null-ls.nvim")]})
                                                   (_G.use "jay-babu/mason-nvim-dap.nvim")]
                                    :keys [(_G.key :<leader>m :<cmd>Mason<cr> {:desc :Mason})]
                                    :cmd :Mason
                                    :config #(let [mason (require :mason)
                                                   mason-lspconfig (require :mason-lspconfig)
                                                   mason-null-ls (require :mason-null-ls)
                                                   mason-nvim-dap (require :mason-nvim-dap)]
                                               (mason.setup)
                                               (mason-lspconfig.setup {:automatic_installation {:exclude [:nil_ls :neocmake :zls]}})
                                               (mason-null-ls.setup {:automatic_installation true})
                                               (mason-nvim-dap.setup {:automatic_installation true}))})]
