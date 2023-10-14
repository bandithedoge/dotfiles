(require :awful.autofocus)

(local assets (require :beautiful.theme_assets))
(local awful (require :awful))
(local beautiful (require :beautiful))
(local gears (require :gears))
(local wibox (require :wibox))

(local keys (require :keys))

; https://github.com/awesomeWM/awesome/issues/1285
(tset package :loaded :naughty.dbus {})

(local join gears.table.join)
(local button awful.button)

(beautiful.init (assets.recolor_layout (require :theme) _G.base05))

(with-open [f (io.open :/etc/hostname :r)]
  (set _G.hostname (f:read)))

(require :layout)
(require :keys)

(root.keys keys.globalkeys)

(awful.spawn.easy_async "xss-lock -- betterlockscreen -l")

;; error handling {{{
(fn error [text]
  (awful.spawn.easy_async (.. :notify-send text)))

(when awesome.startup_errors
  (error awesome.startup_errors))

(do
  (var in-error false)
  (awesome.connect_signal "debug::error"
                          (lambda [err]
                            (when in-error
                              nil)
                            (set in-error true)
                            (error (tostring err))
                            (set in-error false))))

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
