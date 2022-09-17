(require :awful.autofocus)

(local assets (require :beautiful.theme_assets))
(local awful (require :awful))
(local beautiful (require :beautiful))
(local gears (require :gears))
(local naughty (require :naughty))

(local keys (require :keys))

(local join gears.table.join)
(local button awful.button)

(beautiful.init (assets.recolor_layout (require :theme) _G.base05))

(require :layout)
(require :keys)

(root.keys keys.globalkeys)

(awful.spawn.easy_async :hostname #(set _G.hostname $1))
(set naughty.config.presets.critical {:bg _G.base08 :fg _G.base00 :timeout 0})

;; error handling {{{
(fn error [text]
  (naughty.notify {:preset naughty.config.presets.critical :title :FUCK : text}))

(when awesome.startup_errors
  (error awesome.startup_errors))

(do
  (var in_error false)
  (awesome.connect_signal "debug::error"
                          (lambda [err]
                            (when in_error
                              nil)
                            (set in_error true)
                            (error (tostring err))
                            (set in_error false))))

;; }}}

;; rules {{{
(set awful.rules.rules
     [{:rule {}
       :properties {:raise true
                    :screen awful.screen.preferred
                    :focus awful.client.focus.filter
                    :border_width beautiful.border_width
                    :border_color beautiful.border_normal
                    :keys keys.clientkeys
                    :buttons keys.clientbuttons
                    :maximized false
                    :placement (+ awful.placement.no_overlap
                                  awful.placement.no_offscreen
                                  awful.placement.centered)}}])

(set awful.mouse.snap.edge_enabled false)

;; }}}

;; signals {{{
(screen.connect_signal "property::geometry"
                       (lambda [s]
                         (gears.wallpaper.maximized (beautiful.wallpaper s) s
                                                    true)))

(client.connect_signal :manage
                       (lambda [c]
                         (when (and awesome.startup
                                    (not c.size_hints.user_position)
                                    (not c.size_hints.program_position))
                           (awful.placement.no_offscreen c))))

(client.connect_signal "mouse::enter"
                       (lambda [c]
                         (c:emit_signal "request::activate" :mouse_enter
                                        {:raise false})))

(client.connect_signal :focus
                       (lambda [c]
                         (set c.border_color beautiful.border_focus)))

(client.connect_signal :unfocus
                       (lambda [c]
                         (set c.border_color beautiful.border_normal)))

;; }}}
