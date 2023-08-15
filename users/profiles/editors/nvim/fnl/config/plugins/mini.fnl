(_G.use "echasnovski/mini.nvim" {:dependencies [(_G.use :JoosepAlviste/nvim-ts-context-commentstring)]
                                 :lazy false
                                 :keys [(_G.key :<leader>w #(let [bufremove (require :mini.bufremove)] (bufremove.delete)) {:desc "Close buffer"})
                                        (_G.key :<leader><C-w> #(let [bufremove (require :mini.bufremove)] (bufremove.delete 0 true)) {:desc "Close buffer (force)"})]
                                 :config #(let [bufremove (require :mini.bufremove)
                                                mini-comment (require :mini.comment)
                                                pairs (require :mini.pairs)
                                                surround (require :mini.surround)
                                                trailspace (require :mini.trailspace)]
                                           (bufremove.setup {})
                                           (mini-comment.setup {:options {:custom_commentstring (let [ts-context-commentstring (require :ts_context_commentstring.internal)]
                                                                                                  #(or (ts-context-commentstring.calculate_commentstring) vim.bo.commentstring))}})
                                           (pairs.setup {})
                                           (surround.setup {})
                                           (trailspace.setup {}))})
