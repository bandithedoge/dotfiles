(local awful (require :awful))
(local contrib (require :vicious.contrib))
(local dbus (require :dbus_proxy))
(local gears (require :gears))
(local helpers (require :vicious.helpers))
(local naughty (require :naughty))
(local vicious (require :vicious))
(local wibox (require :wibox))

(local join gears.table.join)
(local button awful.button)

(lambda widget [widgets ?args]
  (wibox.widget (join (join {:layout wibox.layout.fixed.horizontal :spacing 5}
                            ?args) widgets)))

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

(lambda [s]
  (set s.mytasklist
       (awful.widget.tasklist {:screen s
                               :filter awful.widget.tasklist.filter.currenttags
                               :buttons (join (button {} 1
                                                      #($1:emit_signal "request::activate"
                                                                       :tasklist
                                                                       {:raise true}))
                                              (button {} 4
                                                      #(awful.client.focus.byidx 1))
                                              (button {} 5
                                                      #(awful.client.focus.byidx -1)))}))
  (set s.mytaglist
       (awful.widget.taglist {:screen s
                              :filter awful.widget.taglist.filter.noempty
                              :buttons (join (button {} 1 #($1:view_only))
                                             (button {} 4
                                                     #(awful.tag.viewnext $1.screen))
                                             (button {} 5
                                                     #(awful.tag.viewprev $1.screen)))}))
  (set s.mylayoutbox (awful.widget.layoutbox s))
  (s.mylayoutbox:buttons (join (button {} 1 #(awful.layout.inc 1))
                               (button {} 3 #(awful.layout.inc -1))
                               (button {} 4 #(awful.layout.inc 1))
                               (button {} 5 #(awful.layout.inc -1))))
  ;; volume {{{
  (set s.myvolumeicon (icon ""))
  (set s.myvolumetext (wibox.widget.textbox))
  (set s.myvolumebox (widget [s.myvolumeicon s.myvolumetext]))
  (let [update-widget (fn []
                        (awful.spawn.easy_async "amixer get Master"
                                                #(let [(volume state) (string.match $1
                                                                                    "%[([%d]+)%%%].*%[([%l]*)%]")]
                                                   (s.myvolumeicon:set_markup (if (= state
                                                                                     :on)
                                                                                  "墳"
                                                                                  (.. "<span foreground='"
                                                                                      _G.base08
                                                                                      "'>"
                                                                                      "婢</span>")))
                                                   (s.myvolumetext:set_markup (if (= state
                                                                                     :on)
                                                                                  (.. volume
                                                                                      "%")
                                                                                  "")))))]
    (update-widget)
    (awful.spawn.with_line_callback (if (= _G.hostname :thonkpad)
                                        "alsactl monitor" "amixer events")
                                    {:stdout update-widget}))
  (s.myvolumebox:connect_signal "button::press"
                                #(match $4
                                   1 (awful.spawn "amixer set Master toggle")
                                   3 (awful.spawn :pavucontrol)
                                   4 (awful.spawn "amixer set Master 5%+")
                                   5 (awful.spawn "amixer set Master 5%-")))
  ;; }}}
  (when (= _G.hostname :thonkpad)
    ;; network {{{
    (set s.mywifiicon (icon ""))
    (set s.mywifibox (wibox.widget.textbox))
    (set s.myneticon (icon ""))
    (set s.mynetworkbox (widget [s.mywifiicon s.mywifibox s.myneticon]))
    (s.mynetworkbox:connect_signal "button::press" #(awful.spawn :connman-gtk))
    (let [p (dbus.Proxy:new {:bus dbus.Bus.SYSTEM
                             :name :net.connman
                             :path :/net/connman/technology/wifi
                             :interface :net.connman.Technology})
          parse-info #(if (= $1 "Not connected.") nil
                          ($1:match "SSID: ([^\\n]*)"))
          update-widget #(awful.spawn.easy_async "iw dev wlp3s0 link"
                                                 #(let [info (parse-info $1)]
                                                    (s.mywifiicon:set_markup (if (= info
                                                                                    nil)
                                                                                 (.. "<span foreground='"
                                                                                     _G.base08
                                                                                     "'>睊</span>")
                                                                                 "直"))
                                                    (s.mywifibox:set_markup (or info
                                                                                ""))))]
      (update-widget)
      (p:connect_signal update-widget :PropertyChanged))
    (let [p (dbus.Proxy:new {:bus dbus.Bus.SYSTEM
                             :name :net.connman
                             :path :/net/connman/technology/ethernet
                             :interface :net.connman.Technology})
          update-widget #(awful.spawn.easy_async "cat /sys/class/net/enp0s25/carrier"
                                                 #(s.myneticon:set_markup (if (= (tonumber $1)
                                                                                 0)
                                                                              (.. "<span foreground='"
                                                                                  _G.base08
                                                                                  "'></span>")
                                                                              "")))]
      (update-widget)
      (p:connect_signal update-widget :PropertyChanged))
    ;; }}}
    ;; battery {{{
    (set s.mybatteryicon (icon ""))
    (set s.mybatterytext (wibox.widget.textbox))
    (set s.mybatterybox (widget [s.mybatteryicon s.mybatterytext]))
    (let [p (dbus.Proxy:new {:bus dbus.Bus.SYSTEM
                             :name :org.freedesktop.UPower
                             :path :/org/freedesktop/UPower/devices/DisplayDevice
                             :interface :org.freedesktop.DBus.Properties})
          update-widget #(let [battery (helpers.pathtotable :/sys/class/power_supply/BAT0)
                               remaining (if battery.charge_now
                                             battery.charge_now
                                             battery.energy_now)
                               capacity (if battery.charge_now
                                            battery.charge_full
                                            battery.energy_full)
                               percentage (math.min (math.floor (* (/ remaining
                                                                      capacity)
                                                                   100))
                                                    100)
                               icons {"Full\n" ""
                                      "Unknown\n" ""
                                      "Charged\n" ""
                                      "Charging\n" ""
                                      "Discharging\n" ""}]
                           (s.mybatteryicon:set_markup (if (<= percentage 15)
                                                           (if (not= battery.status
                                                                     "Charging\n")
                                                               (.. "<span foreground='"
                                                                   _G.base08
                                                                   "'></span>")
                                                               (. icons
                                                                  battery.status))
                                                           (. icons
                                                              battery.status)))
                           (s.mybatterytext:set_markup (.. percentage "%")))]
      (update-widget)
      (p:connect_signal update-widget :PropertiesChanged)))
  ;; }}}
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
                                                            :$1 1 [:coretemp])
                                          (vicious.register (wibox.widget.textbox)
                                                            vicious.widgets.hwmontemp
                                                            "$1°C" 1
                                                            [:coretemp])])
                                 s.mybatterybox
                                 s.myvolumebox
                                 (widget [(icon "")
                                          (vicious.register (wibox.widget.textbox)
                                                            vicious.widgets.date
                                                            "%A %d %B %T" 1)])
                                 (wibox.widget.systray)])])))
