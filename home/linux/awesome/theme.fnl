(let [xresources (require :beautiful.xresources)
      dpi xresources.apply_dpi]
  {:font _G.uiFont
   :bg_normal _G.base00
   :bg_focus _G.base0F
   :bg_urgent _G.base08
   :bg_minimize _G.base03
   :bg_systray _G.base00
   :fg_normal _G.base05
   :fg_focus _G.base00
   :fg_urgent _G.base00
   :fg_minimize _G.base05
   :useless_gap (dpi 5)
   :border_width (dpi 2)
   :border_normal _G.base00
   :border_focus _G.base0F
   :border_marked _G.base05})
