(local awful (require :awful))
(local gears (require :gears))
(local wibox (require :wibox))

(local vicious (require :vicious))

(local join gears.table.join)
(local button awful.button)

(lambda icon [text]
  (wibox.widget {:font (.. _G.monoFont " 14")
                 :widget wibox.widget.textbox
                 : text}))

(set awful.layout.layouts (let [l awful.layout.suit]
                            [l.tile]))

(awful.screen.connect_for_each_screen (lambda [s]
                                        (awful.tag [:1 :2 :3 :4 :5 :6 :7 :8 :9]
                                                   s (. awful.layout.layouts 1))
                                        (set s.mylayoutbox
                                             (awful.widget.layoutbox s))
                                        (s.mylayoutbox:buttons (join (button {}
                                                                             1
                                                                             (lambda []
                                                                               (awful.layout.inc 1)))
                                                                     (button {}
                                                                             3
                                                                             (lambda []
                                                                               (awful.layout.inc -1)))
                                                                     (button {}
                                                                             4
                                                                             (lambda []
                                                                               (awful.layout.inc 1)))
                                                                     (button {}
                                                                             5
                                                                             (lambda []
                                                                               (awful.layout.inc -1)))))
                                        (set s.mytasklist
                                             (awful.widget.tasklist {:screen s
                                                                     :filter awful.widget.tasklist.filter.currenttags}))
                                        (set s.mytaglist
                                             (awful.widget.taglist {:screen s
                                                                    :filter awful.widget.taglist.filter.all}))
                                        (set s.mywibox
                                             (awful.wibar {:position :top
                                                           :screen s
                                                           :bg _G.base00
                                                           :fg _G.base05}))
                                        (s.mywibox:setup (join {:layout wibox.layout.align.horizontal}
                                                               [(join {:layout wibox.layout.fixed.horizontal}
                                                                      [s.mylayoutbox
                                                                       s.mytaglist])
                                                                s.mytasklist
                                                                (join {:layout wibox.layout.fixed.horizontal}
                                                                      [(icon "")
                                                                       (vicious.register (wibox.widget.textbox)
                                                                                         vicious.widgets.date
                                                                                         "%A %d %B %T"
                                                                                         1)])]))))
