(require-macros :hibiscus.vim)

(g! mapleader " ")
(g! maplocalleader "\\")

(let [fold-cycle (require :fold-cycle)]
  (fold-cycle.setup {:softwrap_movement_fix true})
  (map! [n] :<tab> #(fold-cycle.open))
  (map! [n] :<s-tab> #(fold-cycle.close)))

(map! [n] :<cr> ":noh<cr>")
(map! [n] :<bs> ":WhichKey <localleader><cr>")

(map! [n] :K vim.lsp.buf.hover)

(let [wk (require :which-key)
      t (require :telescope.builtin)
      telescope (require :telescope)
      fm (require :fm-nvim)
      mini-br (require :mini.bufremove)
      lsp vim.lsp.buf
      expand_expr (require :expand_expr)
      dapui (require :dapui)
      dap (require :dap)
      neogen (require :neogen)
      trouble (require :trouble)
      icon-picker (require :icon-picker)
      inc-rename (require :inc_rename)]
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
                           :b ["<cmd>:Neotree float buffers<cr>" :Buffers]
                           :F [fm.Lf "File explorer"]
                           :g [fm.Lazygit :Git]
                           :t ["<cmd>:Neotree toggle<cr>" "File tree"]
                           :W ["<cmd>:close<cr>" "Close window"]
                           :w [mini-br.delete "Close buffer"]
                           :T [#(_G.fterm_float:toggle) :Terminal]
                           :i ["<cmd>:PickEverything<cr>" "Insert symbol"]
                           :<C-w> [#(mini-br.delete 0 true)
                                   "Close buffer (force)"]}
                :<localleader> {:a [lsp.code_action "Code actions"]
                                :g [neogen.generate "Generate annotation"]
                                :t [trouble.toggle :Trouble]
                                :j [lsp.definition "Jump to definition"]
                                :D [t.diagnostics :Diagnostics]
                                :e [expand_expr.expand "Expand expression"]
                                :f [#(lsp.format {:async true}) "Format file"]
                                :r [#(inc-rename.rename {:default (vim.fn.expand :<cword>)})
                                    :Rename]
                                :s [lsp.document_symbol :Symbols]
                                :d {:name :Debug
                                    :d [dapui.toggle :Debug]
                                    :e [dapui.eval "Evaluate expression"]
                                    :b [dap.toggle_breakpoint
                                        "Toggle breakpoint"]
                                    :c [dap.continue :Continue]
                                    :o [dap.step_over "Step over"]
                                    :i [dap.step_into "Step into"]}}}))
