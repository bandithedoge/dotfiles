(let [beautiful (require :beautiful)
      xresources (require :beautiful.xresources)
      assets (require :beautiful.theme_assets)
      filesystem (require :gears.filesystem)
      themes (filesystem.get_themes_dir)
      dpi xresources.apply_dpi]
  {:font (.. _G.uiFont " 9")
   :useless_gap (dpi 5)
   :wallpaper _G.wallpaper
   ; common colors
   :bg_normal _G.base00
   :bg_focus _G.base0F
   :bg_urgent _G.base08
   :bg_minimize _G.base03
   :bg_systray _G.base00
   :fg_normal _G.base05
   :fg_focus _G.base00
   :fg_urgent _G.base00
   :fg_minimize _G.base05
   ; borders
   :border_width (dpi 2)
   :border_normal _G.base00
   :border_focus _G.base0F
   :border_marked _G.base05
   ; taglist widget
   :taglist_fg_focus _G.base00
   :taglist_bg_focus _G.base0F
   :taglist_fg_urgent _G.base00
   :taglist_bg_urgent _G.base08
   :taglist_bg_occupied _G.base02
   :taglist_fg_occupied _G.base03
   :taglist_font (.. _G.monoFont " 9")
   ; tasklist widget
   :tasklist_fg_normal _G.base04
   :tasklist_bg_normal _G.base02
   :tasklist_fg_focus _G.base00
   :tasklist_bg_focus _G.base0F
   :tasklist_fg_urgent _G.base00
   :tasklist_bg_urgent _G.base08
   ; graphs
   :graph_bg _G.base02
   :graph_fg _G.base0F
   ; notifications
   :notification_bg _G.base02
   :notification_border_width 2
   :notification_border_color _G.base0F
   ; layout icons
   :layout_tile (.. themes :default/layouts/tilew.png)
   :layout_max (.. themes :default/layouts/maxw.png)})
