(import-macros {: tx!} :config.macros)

(local astal (require :astal))
(local gtk3 (require :astal.gtk3))
(local Widget (require :astal.gtk3.widget))

(local json (require :cjson))

(local M {})

(local windows [])
(local workspaces (astal.Variable []))

(local focused (astal.Variable {:title "" :app_id ""}))

(local process (astal.subprocess "niri msg -j event-stream"
                                 #(let [res (json.decode $1)]
                                    (if res.WorkspacesChanged
                                        (workspaces:set res.WorkspacesChanged.workspaces)
                                        res.WindowsChanged
                                        (each [_ window (ipairs res.WindowsChanged.windows)]
                                          (tset windows window.id window))
                                        res.WindowOpenedOrChanged
                                        (let [window res.WindowOpenedOrChanged.window]
                                          (tset windows window.id window)
                                          (when window.is_focused
                                            (focused:set window)))
                                        res.WindowClosed
                                        (tset windows res.WindowClosed.id nil)
                                        res.WindowFocusChanged
                                        (focused:set (let [id res.WindowFocusChanged.id]
                                                       (if (not= id json.null)
                                                           (. windows id)
                                                           {:title ""
                                                            :app_id ""})))))
                                 #nil))

(local icon-theme (gtk3.Gtk.IconTheme.get_default))

(lambda M.NiriWindow [monitor]
  (Widget.Box (tx! (Widget.Icon {:icon (: (astal.bind focused) :as #$1.app_id)
                                 :visible (: (astal.bind focused) :as
                                             #(icon-theme:has_icon $1.app_id))})
                   (Widget.Label {:label (: (astal.bind focused) :as #$1.title)})
                   {:class_name :niri-window})))

M
