{
  pkgs,
  inputs,
  lib,
  ...
}: let
  rofi-stuff = pkgs.callPackage ../rofi {};
in {
  home.packages = with pkgs; [
    grimblast
  ];

  wayland.windowManager.hyprland = {
    # {{{
    enable = true;
    systemd.variables = ["--all"];
    plugins = [
      inputs.hyprsplit.packages.${pkgs.system}.hyprsplit
    ];
    settings = let
      color = c: "rgb(${pkgs.lib.removePrefix "#" c})";
      mod = "SUPER";
    in
      with pkgs.rice; {
        general = {
          border_size = 2;
          gaps_in = 5;
          gaps_out = 10;
          "col.inactive_border" = color base00;
          "col.active_border" = color base0F;
          layout = "master";
          allow_tearing = true;
        };
        decoration = {
          blur = {
            enabled = false;
            new_optimizations = true;
            xray = true;
          };
          shadow = {
            range = 8;
            render_power = 1;
            color = "0x80000000";
          };
        };
        animations = {
          first_launch_animation = false;
        };
        input = {
          kb_layout = "pl";
          repeat_delay = 300;
          accel_profile = "flat";
          touchpad = {
            disable_while_typing = false;
            tap-to-click = false;
            clickfinger_behavior = true;
          };
        };
        misc = {
          disable_hyprland_logo = true;
          disable_splash_rendering = true;
          force_default_wallpaper = -1;
          mouse_move_enables_dpms = true;
          key_press_enables_dpms = true;
          allow_session_lock_restore = true;
          vrr = lib.mkDefault 0;
        };
        master.mfact = 0.5;
        xwayland.force_zero_scaling = true;
        opengl.force_introspection = 1;
        render.expand_undersized_textures = false;
        cursor = {
          persistent_warps = true;
          warp_on_change_workspace = true;
        };
        monitor = [
          ", preferred, auto, 1"
          "DP-1, preferred, 0x0, 1"
          "DP-2, preferred, 1920x60, 1"
          "DP-3, preferred, 3840x-400, 1, transform, 1"
        ];
        workspace = [
          "m[DP-3], layoutopt:orientation:top"
        ];
        exec = [
          "systemctl --user restart waybar"
        ];
        exec-once = [
          "swaybg -i ${wallpaper} -m fill"
        ];
        bind =
          [
            "${mod}, return, exec, ${terminal}"
            "${mod}, space, exec, ${menu}"
            "${mod}, backspace, exec, wlr-which-key"
            "${mod} CTRL, p, exec, ${rofi-stuff}/bin/keepass"
            ", Print, exec, ${pkgs.writeShellScript "screenshot" "grimblast --freeze save area - | satty -f -"}"

            "${mod}, w, killactive"
            "${mod}, t, togglefloating"
            "${mod}, f, fullscreen"
            "${mod} CTRL, q, exec, loginctl kill-session $XDG_SESSION_ID"
            "${mod} CTRL, r, exec, hyprctl reload"
          ]
          ++ pkgs.lib.flatten (map (x: let
            x' = toString x;
          in [
            "${mod}, ${x'}, split:workspace, ${x'}"
            "${mod} SHIFT, ${x'}, split:movetoworkspacesilent, ${x'}"
          ]) (pkgs.lib.range 1 9));
        binde = [
          "${mod}, j, layoutmsg, cyclenext"
          "${mod}, k, layoutmsg, cycleprev"
          "${mod} SHIFT, j, layoutmsg, swapnext"
          "${mod} SHIFT, k, layoutmsg, swapprev"
          "${mod}, h, focusmonitor, -1"
          "${mod}, l, focusmonitor, +1"
          "${mod} SHIFT, h, movewindow, mon:-1"
          "${mod} SHIFT, l, movewindow, mon:+1"
          "${mod} CTRL, h, resizeactive, -20 0"
          "${mod} CTRL, j, resizeactive, 0 20"
          "${mod} CTRL, k, resizeactive, 0 -20"
          "${mod} CTRL, l, resizeactive, 20 0"
          ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
          ", XF86AudioRaiseVolume, exec, wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+"
          ", XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
          ", XF86AudioPlay, exec, playerctl -p strawberry play-pause"
          ", XF86AudioPrev, exec, playerctl -p strawberry previous"
          ", XF86AudioNext, exec, playerctl -p strawberry next"
        ];
        bindm = [
          "${mod}, mouse:272, movewindow"
          "${mod}, mouse:273, resizewindow"
        ];
        bindn = [
          "CTRL, space, exec, makoctl dismiss"
          "CTRL SHIFT, space, exec, makoctl dismiss -a"
        ];
        bezier = "easeOutExpo, 0.16, 1, 0.3, 1";
        animation = [
          "global, 1, 2, easeOutExpo"
          "windows, 0"
          "workspaces, 1, 2, easeOutExpo, slidefade"
        ];
        windowrulev2 =
          [
            "tile, title:(Adobe Photoshop 2021)|(Adobe Illustrator 2021)|(Guitar Pro 8)|(DaVinci Resolve Studio)"
            "immediate, class:(gamescope)"
            "nearestneighbor 1, class:(gamescope)"
            "nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0"
          ]
          ++ map (x: "opacity 0.95 0.85, class:(${x})") [
            terminal
            "emacs"
          ];
      };
  }; # }}}
}
