(import-macros {: tx!} :macros)

(local astal (require :astal))
(local App (require :astal.gtk3.app))
(local Widget (require :astal.gtk3.widget))
(local gtk3 (require :astal.gtk3))

(App:start {:instance_name :bar
            :main #(each [_ gdkmonitor (pairs App.monitors)]
                     (Widget.Window (tx! (Widget.Label {:label :KURWAAAAAAAAAAA})
                                         {: gdkmonitor
                                          :anchor (let [Anchor gtk3.Astal.WindowAnchor]
                                                    (+ Anchor.TOP Anchor.LEFT
                                                       Anchor.RIGHT))
                                          :exclusivity :EXCLUSIVE})))})
