(set vim.g.mapleader " ")
(set vim.g.maplocalleader "\\")

(vim.keymap.set :n :<cr> ":noh<cr>" {:silent true})
(vim.keymap.set :n :<s-tab> :zA {:silent true})
(vim.keymap.set :n :<tab> :za {:silent true})
(vim.keymap.set :n :j :gj {:silent true})
(vim.keymap.set :n :k :gk {:silent true})
(vim.keymap.set :n :<bs> ":WhichKey <localleader><cr>"
                {:noremap true :silent true})

(vim.keymap.set :n :K vim.lsp.buf.hover {:silent true})

(let [wk (require :which-key)
      t (require :telescope.builtin)
      telescope (require :telescope)
      fm (require :fm-nvim)
      nvim-tree (require :nvim-tree)
      mini-br (require :mini.bufremove)
      lsp vim.lsp.buf
      expand_expr (require :expand_expr)
      dapui (require :dapui)
      dap (require :dap)]
  (wk.setup {:ignore_missing true :icons {:separator ""}})
  (wk.register {:<leader> {:<space> [t.commands "Enter command"]
                           :f {:name :Find
                               :f [telescope.extensions.frecency.frecency
                                   :Files]
                               :h [t.help_tags :Help]
                               :H [t.highlights "Highlight groups"]
                               :t [t.builtin :Telescope]}
                           :o {:name :Open
                               :d ["<cmd>:cd ~/dotfiles<cr>" :Dotfiles]
                               :g ["<cmd>:cd ~/git<cr>" :Git]
                               :s ["<cmd>:cd ~/sql<cr>" :School]}
                           :b [t.buffers :Buffers]
                           :F [fm.Lf "File explorer"]
                           :g [fm.Lazygit :Git]
                           :t [nvim-tree.toggle "File tree"]
                           :W ["<cmd>:close<cr>" "Close window"]
                           :w [mini-br.delete "Close buffer"]
                           :? ["<cmd>:Cheatsheet<cr>" :Cheatsheet]
                           :T [(lambda []
                                 (fterm_float:toggle))
                               :Terminal]
                           :<C-w> [(lambda []
                                     mini-br.delete
                                     0
                                     true)
                                   "Close buffer (force)"]}
                :<localleader> {:a [lsp.code_action "Code actions"]
                                :j [lsp.definition "Jump to definition"]
                                :D [t.diagnostics :Diagnostics]
                                :e [expand_expr.expand "Expand expression"]
                                :f [lsp.formatting "Format file"]
                                :r [lsp.rename :Rename]
                                :s [lsp.document_symbol :Symbols]
                                :d {:name :Debug
                                    :d [dapui.toggle :Debug]
                                    :e [dapui.eval "Evaluate expression"]
                                    :b [dap.toggle_breakpoint
                                        "Toggle breakpoint"]
                                    :c [dap.continue :Continue]
                                    :o [dap.step_over "Step over"]
                                    :i [dap.step_into "Step into"]}}}))
