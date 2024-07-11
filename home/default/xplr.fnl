(local xplr xplr)

(set xplr.config.general.enable_mouse true)
(set xplr.config.general.show_hidden true)

(set xplr.config.general.focus_ui.style {:bg :DarkGray})

(let [tree-view (require :tree-view)]
  (tree-view.setup {:as_default_layout true :indent "│ "}))

(let [fzf (require :fzf)]
  (fzf.setup {:bin :sk}))

(let [map (require :map)]
  (map.setup {}))

(let [web-devicons (require :web-devicons)]
  (web-devicons.setup {}))

(let [wl-clipboard (require :wl-clipboard)]
  (wl-clipboard.setup {}))

(let [dragon (require :dragon)]
  (dragon.setup {}))

(let [ouch (require :ouch)]
  (ouch.setup {}))

(let [command-mode (require :command-mode)]
  (command-mode.setup {}))

{}

