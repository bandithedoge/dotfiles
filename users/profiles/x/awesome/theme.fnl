(let [beautiful (require :beautiful)
      xresources (require :beautiful.xresources)
      assets (require :beautiful.theme_assets)
      filesystem (require :gears.filesystem)
      themes (filesystem.get_themes_dir)
      dpi xresources.apply_dpi]
  {:font (.. _G.uiFont " 10")
   :useless_gap (dpi 5)
   :wallpaper #(assets.wallpaper _G.base00 _G.base05 _G.base0F $1)
   ;; common colors
   :bg_normal _G.base00
   :bg_focus _G.base0F
   :bg_urgent _G.base08
   :bg_minimize _G.base03
   :bg_systray _G.base00
   :fg_normal _G.base05
   :fg_focus _G.base00
   :fg_urgent _G.base00
   :fg_minimize _G.base05
   ;; borders
   :border_normal _G.base00
   :border_focus _G.base0F
   :border_marked _G.base05
   :border_width (dpi 2)
   ;; taglist widget
   :taglist_bg_occupied _G.base02
   :taglist_fg_occupied _G.base03
   :taglist_font (.. _G.monoFont " 8")
   ;; layout icons
   :layout_tile (.. themes :default/layouts/tilew.png)})

;; theme.layout_fairh = themes_path.."default/layouts/fairhw.png"
;; theme.layout_fairv = themes_path.."default/layouts/fairvw.png"
;; theme.layout_floating  = themes_path.."default/layouts/floatingw.png"
;; theme.layout_magnifier = themes_path.."default/layouts/magnifierw.png"
;; theme.layout_max = themes_path.."default/layouts/maxw.png"
;; theme.layout_fullscreen = themes_path.."default/layouts/fullscreenw.png"
;; theme.layout_tilebottom = themes_path.."default/layouts/tilebottomw.png"
;; theme.layout_tileleft   = themes_path.."default/layouts/tileleftw.png"
;; theme.layout_tile = themes_path.."default/layouts/tilew.png"
;; theme.layout_tiletop = themes_path.."default/layouts/tiletopw.png"
;; theme.layout_spiral  = themes_path.."default/layouts/spiralw.png"
;; theme.layout_dwindle = themes_path.."default/layouts/dwindlew.png"
;; theme.layout_cornernw = themes_path.."default/layouts/cornernww.png"
;; theme.layout_cornerne = themes_path.."default/layouts/cornernew.png"
;; theme.layout_cornersw = themes_path.."default/layouts/cornersww.png"
;; theme.layout_cornerse = themes_path.."default/layouts/cornersew.png"
