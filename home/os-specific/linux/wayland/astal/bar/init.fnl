(import-macros {: tx!} :config.macros)

(local astal (require :astal))
(local Astal (astal.require :Astal))
(local App (require :astal.gtk3.app))
(local Widget (require :astal.gtk3.widget))

(lambda SysTray []
  (Widget.Box (tx! (let [Tray (astal.require :AstalTray)
                         tray (Tray.get_default)]
                     (: (astal.bind tray :items) :as
                        (fn [items]
                          (icollect [_ item (ipairs items)]
                            (Widget.MenuButton (tx! (Widget.Icon {:gicon (astal.bind item
                                                                                     :gicon)})
                                                    {:tooltip_markup (astal.bind item
                                                                                 :tooltip_markup)
                                                     :use_popover false
                                                     :menu_model (astal.bind item
                                                                             :menu-model)
                                                     :action_group (: (astal.bind item
                                                                                  :action-group)
                                                                      :as
                                                                      #[:dbusmenu
                                                                        $1])}))))))
                   {:class_name :tray})))

(lambda Bar [monitor]
  (Widget.Window (tx! (Widget.CenterBox [(Widget.Box (let [niri (require :niri)]
                                                       (tx! (niri.NiriWindow monitor)
                                                            {:halign :START})))
                                         (Widget.Box [])
                                         (Widget.Box (tx! (SysTray)
                                                          {:halign :END}))])
                      {:class_name :bar
                       :gdkmonitor monitor
                       :anchor (let [Anchor Astal.WindowAnchor]
                                 (+ Anchor.TOP Anchor.LEFT Anchor.RIGHT))
                       :exclusivity :EXCLUSIVE})))

(App:start {:main #(each [_ mon (pairs App.monitors)]
                     (Bar mon))
            :css (astal.read_file (let [str (: (. (debug.getinfo 2 :S) :source)
                                               :sub 2)]
                                    (.. (str:match "(.*/)") :style.css)))})
