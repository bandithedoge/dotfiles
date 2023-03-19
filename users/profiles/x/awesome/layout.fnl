(local awful (require :awful))
(local beautiful (require :beautiful))
(local gears (require :gears))

(set awful.layout.layouts (let [l awful.layout.suit]
                            [l.tile]))

(awful.screen.connect_for_each_screen (lambda [s]
                                        (gears.wallpaper.maximized beautiful.wallpaper
                                                                   s true)
                                        (awful.tag [:1
                                                    :2
                                                    :3
                                                    :4
                                                    :5
                                                    :6
                                                    :7
                                                    :8
                                                    :9]
                                                   s
                                                   (. awful.layout.layouts 1))
                                        ((require :bar) s)))
