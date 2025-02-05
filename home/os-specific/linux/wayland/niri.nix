{pkgs, ...}: {
  home.packages = with pkgs; [
    niri
    sway-contrib.grimshot
  ];

  xdg.configFile."niri/config.kdl".text = with pkgs.rice; ''
    spawn-at-startup "swaybg" "-i" "${wallpaper}" "-m" "fill"
    spawn-at-startup "${with pkgs; lib.getExe xwayland-satellite}"

    environment {
      DISPLAY ":0"
    }

    binds {
      Mod+Shift+Slash { show-hotkey-overlay; }
      Mod+Return { spawn "${terminal}"; }
      Mod+Space { spawn "${pkgs.lib.concatStringsSep "\" \"" (pkgs.lib.splitString " " menu)}"; }
      Mod+Backspace { spawn "wlr-which-key"; }
      Mod+Ctrl+p { spawn "keepmenu"; }
      Mod+Ctrl+q { spawn "loginctl" "terminate-user" ""; }

      Print {
    	spawn "${pkgs.writeShellScript "screenshot" "grimshot save area - | satty -f -"}"
      }
      Shift+Print {
    	spawn "${pkgs.writeShellScript "save-replay" "killall -SIGUSR1 gpu-screen-recorder && notify-send -u low 'GPU Screen Recorder' 'Replay saved' -i com.dec05eba.gpu_screen_recorder -a 'GPU Screen Recorder'"}";
      }

      XF86AudioMute { spawn "wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"; }
      XF86AudioRaiseVolume { spawn "wpctl" "set-volume" "-l" "1.5" "@DEFAULT_AUDIO_SINK@" "5%+"; }
      XF86AudioLowerVolume { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%-"; }
      XF86AudioPlay { spawn "playerctl" "-p" "strawberry" "play-pause"; }
      XF86AudioPrev { spawn "playerctl" "-p" "strawberry" "previous"; }
      XF86AudioNext { spawn "playerctl" "-p" "strawberry" "next"; }

      Mod+h { focus-monitor-previous; }
      Mod+j { focus-window-down-or-column-right; }
      Mod+k { focus-window-up-or-column-left; }
      Mod+l { focus-monitor-next; }
      Mod+Shift+h { move-window-to-monitor-previous; }
      Mod+Shift+j { swap-window-right; }
      Mod+Shift+k { swap-window-left; }
      Mod+Shift+l { move-window-to-monitor-next; }
      Mod+Shift+t { switch-focus-between-floating-and-tiling; }

      Mod+r { switch-preset-column-width; }
      Mod+t { toggle-window-floating; }
      Mod+w { close-window; }
      Mod+Comma { consume-window-into-column; }
      Mod+Period { expel-window-from-column; }

      Mod+WheelScrollDown { focus-workspace-down; }
      Mod+WheelScrollUp { focus-workspace-up; }

      Mod+1 { focus-workspace 1; }
      Mod+2 { focus-workspace 2; }
      Mod+3 { focus-workspace 3; }
      Mod+4 { focus-workspace 4; }
      Mod+5 { focus-workspace 5; }
      Mod+6 { focus-workspace 6; }
      Mod+7 { focus-workspace 7; }
      Mod+8 { focus-workspace 8; }
      Mod+9 { focus-workspace 9; }
      Mod+Shift+1 { move-window-to-workspace 1; }
      Mod+Shift+2 { move-window-to-workspace 2; }
      Mod+Shift+3 { move-window-to-workspace 3; }
      Mod+Shift+4 { move-window-to-workspace 4; }
      Mod+Shift+5 { move-window-to-workspace 5; }
      Mod+Shift+6 { move-window-to-workspace 6; }
      Mod+Shift+7 { move-window-to-workspace 7; }
      Mod+Shift+8 { move-window-to-workspace 8; }
      Mod+Shift+9 { move-window-to-workspace 9; }
    }

    hotkey-overlay {
      skip-at-startup
    }

    input {
      focus-follows-mouse
      keyboard {
    	repeat-delay 300
    	xkb { layout "pl"; }
      }
      mouse { accel-profile "flat"; }
      touchpad {
    	natural-scroll
    	scroll-method "two-finger"
      }
    }

    layout {
      always-center-single-column
      gaps 10
      default-column-width {}
      focus-ring {
    	active-color "${base0F}"
    	inactive-color "${base00}"
    	width 2
      }
      insert-hint {
    	color "${base0F}"
      }
      preset-column-widths {
    	proportion 0.3
    	proportion 0.5
    	proportion 0.7
    	proportion 1.0
      }
      struts {
        left 10
        right 10
      }
    }

    output "DP-2" {
      position x=0 y=0
    }
    output "DP-3" {
      position x=1920 y=0
    }

    window-rule {
      match app-id=r#"^com\.mitchellh\.ghostty$"# is-focused=false
      opacity 0.85
      draw-border-with-background false
    }

    window-rule {
      match app-id=r#"^com\.mitchellh\.ghostty$"# is-focused=true
      opacity 0.95
      draw-border-with-background false
    }

    layer-rule {
      match namespace="notifications"
      block-out-from "screencast"
    }

    prefer-no-csd
  '';

  # xdg.configFile."niri/config.kdl".text = lib.hm.generators.toKDL {} {
  #   input = {
  #     keyboard = {
  #       xkb.layout = "pl";
  #       repeat-delay = 300;
  #     };
  #     touchpad = {
  #       natural-scroll = [];
  #       scroll-method = "two-finger";
  #     };
  #     mouse.accel-profile = "flat";
  #     focus-follows-mouse = [];
  #   };
  #   "output \"DP-2\""."position x=0 y=0" = [];
  #   "output \"DP-3\""."position x=1920 y=0" = [];
  #   layout = {
  #     gaps = 10;
  #     # center-focused-column = "on-overflow";
  #     always-center-single-column = [];
  #     focus-ring = {
  #       width = 2;
  #       active-color = pkgs.rice.base0F;
  #       inactive-color = pkgs.rice.base00;
  #     };
  #     preset-column-widths = {
  #       "proportion 0.3" = [];
  #       "proportion 0.5" = [];
  #       "proportion 0.7" = [];
  #       "proportion 1.0" = [];
  #     };
  #     default-column-width = {};
  #     insert-hint.color = pkgs.rice.base0F;
  #   };
  #   prefer-no-csd = [];
  #   binds =
  #     {
  #       "Mod+Shift+Slash".show-hotkey-overlay = [];
  #       "Mod+Return".spawn = pkgs.rice.terminal;
  #       "Mod+w".close-window = [];
  #       "Mod+Ctrl+q".spawn = ["loginctl" "terminate-user" ""];
  #       "Mod+Space".spawn = pkgs.lib.splitString " " pkgs.rice.menu;
  #       "Mod+Backspace".spawn = "wlr-which-key";
  #       "Print".screenshot = [];
  #       "Mod+Shift+Print".spawn = builtins.toString (pkgs.writeShellScript "save-replay" "killall -SIGUSR1 gpu-screen-recorder && notify-send -u low 'GPU Screen Recorder' 'Replay saved' -i com.dec05eba.gpu_screen_recorder -a 'GPU Screen Recorder'");
  #
  #       "Mod+j".focus-window-down-or-column-right = [];
  #       "Mod+k".focus-window-up-or-column-left = [];
  #       "Mod+h".focus-monitor-previous = [];
  #       "Mod+l".focus-monitor-next = [];
  #       "Mod+Shift+h".move-column-left = [];
  #       "Mod+Shift+j".move-window-down = [];
  #       "Mod+Shift+k".move-window-up = [];
  #       "Mod+Shift+l".move-column-right = [];
  #
  #       "Mod+Comma".consume-window-into-column = [];
  #       "Mod+Period".expel-window-from-column = [];
  #       "Mod+t".toggle-window-floating = [];
  #       "Mod+Shift+t".switch-focus-between-floating-and-tiling = [];
  #       "Mod+r".switch-preset-column-width = [];
  #     }
  #     // (builtins.listToAttrs
  #       (builtins.map (x:
  #         pkgs.lib.nameValuePair
  #         "Mod+${builtins.toString x}"
  #         {focus-workspace = x;}) (pkgs.lib.range 1 9)))
  #     // builtins.listToAttrs
  #     (builtins.map (x:
  #       pkgs.lib.nameValuePair
  #       "Mod+Shift+${builtins.toString x}"
  #       {move-window-to-workspace = x;}) (pkgs.lib.range 1 9));
  #   hotkey-overlay.skip-at-startup = [];
  #   spawn-at-startup = ["swaybg" "-i" "${pkgs.rice.wallpaper}" "-m" "fill"];
  # };
}
