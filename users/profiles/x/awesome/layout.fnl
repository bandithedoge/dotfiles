(local awful (require :awful))
(local contrib (require :vicious.contrib))
(local gears (require :gears))
(local vicious (require :vicious))
(local wibox (require :wibox))

(local join gears.table.join)
(local button awful.button)

(fn icon [text]
  (wibox.widget {:font (.. _G.monoFont " 12")
                 :widget wibox.widget.textbox
                 :forced_width 20
                 :align :center
                 : text}))

(fn graph []
  (let [w (wibox.widget.graph)]
    (w:set_width 40)
    w))

(lambda widget [widgets ?args]
  (wibox.widget (join (join {:layout wibox.layout.fixed.horizontal :spacing 5}
                            ?args) widgets)))

(set awful.layout.layouts (let [l awful.layout.suit]
                            [l.tile]))

(let [f (lambda [s]
          (awful.tag [:1 :2 :3 :4 :5 :6 :7 :8 :9] s (. awful.layout.layouts 1))
          (set s.mytasklist
               (awful.widget.tasklist {:screen s
                                       :filter awful.widget.tasklist.filter.currenttags
                                       :buttons (join (button {} 1
                                                              #($1:emit_signal "request::activate"
                                                                               :tasklist
                                                                               {:raise true}))
                                                      (button {} 4 #(awful.client.focus.byidx 1))
                                                      (button {} 5 #(awful.client.focus.byidx -1)))}))
          (set s.mytaglist
               (awful.widget.taglist {:screen s
                                      :filter awful.widget.taglist.filter.noempty
                                      :buttons (join (button {} 1
                                                             #($1:view_only))
                                                     (button {} 4
                                                             #(awful.tag.viewnext $1.screen))
                                                     (button {} 5
                                                             #(awful.tag.viewprev $1.screen)))}))
          (set s.mylayoutbox (awful.widget.layoutbox s))
          (s.mylayoutbox:buttons (join (button {} 1 #(awful.layout.inc 1))
                                       (button {} 3 #(awful.layout.inc -1))
                                       (button {} 4 #(awful.layout.inc 1))
                                       (button {} 5 #(awful.layout.inc -1))))
          (set s.myvolumebox
               (widget [(vicious.cache vicious.widgets.volume)
                        (vicious.register (icon nil) vicious.widgets.volume
                                          #(if (= (. $2 2) "🔈")
                                               (.. "<span foreground='"
                                                   _G.base08 "'>ﱝ</span>")
                                               "墳")
                                          1 :Master)
                        (vicious.register (wibox.widget.textbox)
                                          vicious.widgets.volume
                                          #(if (= (. $2 2) "🔈") ""
                                               (.. (. $2 1) "%"))
                                          1 :Master)]))
          (s.myvolumebox:connect_signal "button::press"
                                        #(match $4
                                           1 (awful.spawn "amixer set Master toggle")
                                           3 (awful.spawn :pavucontrol)
                                           4 (awful.spawn "amixer set Master 5%+")
                                           5 (awful.spawn "amixer set Master 5%-")))
          (set s.mynetworkbox
               (widget [(widget [(vicious.cache vicious.widgets.net)
                                 (vicious.register (icon nil)
                                                   vicious.widgets.net
                                                   #(if (= (. $2
                                                              "{enp0s25 carrier}")
                                                           1)
                                                        ""
                                                        (.. "<span foreground='"
                                                            _G.base08
                                                            "'></span>"))
                                                   5)])
                        (widget [(vicious.cache vicious.widgets.wifi)
                                 (vicious.register (icon nil)
                                                   vicious.widgets.wifi
                                                   #(if (= (. $2 "{ssid}") :N/A)
                                                        (.. "<span foreground='"
                                                            _G.base08
                                                            "'>睊</span>")
                                                        "直")
                                                   5 :wlp3s0)
                                 (vicious.register (wibox.widget.textbox)
                                                   vicious.widgets.wifi
                                                   #(if (= (. $2 "{ssid}") :N/A)
                                                        "" (. $2 "{ssid}"))
                                                   5 :wlp3s0)])]))
          (s.mynetworkbox:connect_signal "button::press"
                                         #(awful.spawn :connman-gtk))
          (set s.mywibox (awful.wibar {:position :top
                                       :screen s
                                       :bg _G.base00
                                       :fg _G.base05}))
          (s.mywibox:setup (join {:layout wibox.layout.align.horizontal}
                                 [(join {:layout wibox.layout.fixed.horizontal}
                                        [s.mytaglist s.mylayoutbox])
                                  s.mytasklist
                                  (join {:layout wibox.layout.fixed.horizontal
                                         :spacing 10}
                                        [s.mynetworkbox
                                         (widget [(icon "﬙")
                                                  (vicious.cache vicious.widgets.cpu)
                                                  (vicious.register (graph)
                                                                    vicious.widgets.cpu
                                                                    :$1 1)
                                                  (vicious.register (wibox.widget.textbox)
                                                                    vicious.widgets.cpu
                                                                    "$1%" 1)])
                                         (widget [(icon "")
                                                  (vicious.cache vicious.widgets.mem)
                                                  (vicious.register (graph)
                                                                    vicious.widgets.mem
                                                                    :$1 1)
                                                  (vicious.register (wibox.widget.textbox)
                                                                    vicious.widgets.mem
                                                                    "$2 MB" 1)])
                                         (widget [(icon "﨎")
                                                  (vicious.cache vicious.widgets.hwmontemp)
                                                  (vicious.register (graph)
                                                                    vicious.widgets.hwmontemp
                                                                    :$1 1
                                                                    [:coretemp])
                                                  (vicious.register (wibox.widget.textbox)
                                                                    vicious.widgets.hwmontemp
                                                                    "$1°C" 1
                                                                    [:coretemp])])
                                         s.myvolumebox
                                         (widget [(vicious.register (icon nil)
                                                                    contrib.ac
                                                                    #(if (= (. $2
                                                                               1)
                                                                            :On)
                                                                         ""
                                                                         (<= (. $2
                                                                                2)
                                                                             15)
                                                                         (.. "<span foreground='"
                                                                             _G.base08
                                                                             "'></span>")
                                                                         "")
                                                                    5 :AC)
                                                  (vicious.register (wibox.widget.textbox)
                                                                    vicious.widgets.bat
                                                                    #(let [level (. $2
                                                                                    2)]
                                                                       (if (>= level
                                                                               98)
                                                                           :Full
                                                                           (<= level
                                                                               15)
                                                                           (.. "<span foreground='"
                                                                               _G.base08
                                                                               "'>"
                                                                               level
                                                                               "%</span>")
                                                                           (.. level "%")))
                                                                    5 :BAT0)])
                                         (widget [(icon "")
                                                  (vicious.register (wibox.widget.textbox)
                                                                    vicious.widgets.date
                                                                    "%A %d %B %T"
                                                                    1)])
                                         (wibox.widget.systray)])])))]
  (awful.screen.connect_for_each_screen f))
