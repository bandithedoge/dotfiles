(require-macros :hibiscus.vim)

(g! mapleader " ")
(g! maplocalleader "\\")

(let [fold-cycle (require :fold-cycle)]
  (fold-cycle.setup {:softwrap_movement_fix true})
  (map! [n] :<tab> #(fold-cycle.open)))

(map! [n] :<cr> ":noh<cr>")
(map! [n] :<bs> ":WhichKey <localleader><cr>")
(map! [n] :<s-tab> ":CybuLastusedNext<cr>")

(map! [n] :K vim.lsp.buf.hover)

(let [wk (require :which-key)]
  (wk.setup {:ignore_missing false :icons {:separator "󰍟"} :show_help false})
  (wk.register (let [t (require :telescope.builtin)
                     telescope (require :telescope)
                     fm (require :fm-nvim)
                     mini-br (require :mini.bufremove)
                     lsp vim.lsp.buf
                     expand_expr (require :expand_expr)
                     dapui (require :dapui)
                     dap (require :dap)
                     neogen (require :neogen)
                     trouble (require :trouble)
                     icon-picker (require :icon-picker)]
                 {:<leader> {:<space> [t.commands "Enter command"]
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
                             :t ["<cmd>:Neotree toggle<cr>" "File tree"]
                             :W ["<cmd>:close<cr>" "Close window"]
                             :w [mini-br.delete "Close buffer"]
                             :T [#(_G.fterm_float:toggle) :Terminal]
                             :i ["<cmd>:IconPickerNormal alt_font emoji nerd_font nerd_font_v3 symbols<cr>"
                                 "Insert symbol"]
                             :<C-w> [#(mini-br.delete 0 true)
                                     "Close buffer (force)"]
                             :h [telescope.extensions.yank_history.yank_history
                                 "Yank history"]}
                  :<localleader> {:a [lsp.code_action "Code actions"]
                                  :g [neogen.generate "Generate annotation"]
                                  :t [trouble.toggle :Trouble]
                                  :D ["<cmd>:Glance definitions<cr>"
                                      "Show definitions"]
                                  :e [expand_expr.expand "Expand expression"]
                                  :f [lsp.format "Format file"]
                                  :r [lsp.rename :Rename]
                                  :s [lsp.document_symbol :Symbols]
                                  :d {:name :Debug
                                      :d [dapui.toggle :Debug]
                                      :e [dapui.eval "Evaluate expression"]
                                      :b [dap.toggle_breakpoint
                                          "Toggle breakpoint"]
                                      :c [dap.continue :Continue]
                                      :o [dap.step_over "Step over"]
                                      :i [dap.step_into "Step into"]}
                                  :l {:name :Language}}})))
