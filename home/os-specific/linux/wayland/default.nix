{
  pkgs,
  config,
  ...
}: let
  rofi-stuff = pkgs.callPackage ../rofi {};
in {
  home = {
    packages = with pkgs; [
      autotiling-rs
      grimblast
      satty
      swaybg
      swaysome
      wev
      wl-clipboard
      wlr-which-key
    ];
    sessionVariables = {
      GDK_BACKEND = "wayland,x11";
      NIXOS_OZONE_WL = "1";
      QT_QPA_PLATFORM = "wayland;xcb";
      SDL_VIDEODRIVER = "wayland";
      _JAVA_AWT_WM_NONREPARENTING = "1";
    };
  };

  wayland.windowManager.sway = with pkgs.rice; {
    # {{{
    enable = true;
    package = pkgs.swayfx;
    config = {
      bars = [
        {command = "waybar";}
      ];
      inherit terminal menu;
      colors = {
        background = base00;
        focused = {
          background = base00;
          border = base0F;
          childBorder = base0F;
          indicator = base0F;
          text = base0F;
        };
        focusedInactive = {
          background = base00;
          border = base00;
          childBorder = base00;
          indicator = base00;
          text = base0F;
        };
        unfocused = {
          background = base00;
          border = base00;
          childBorder = base00;
          indicator = base00;
          text = base05;
        };
        urgent = {
          background = base08;
          border = base08;
          childBorder = base08;
          indicator = base08;
          text = base00;
        };
      };
      fonts = {
        names = with pkgs.rice; [uiFont monoFont];
        size = 11.0;
      };
      gaps = {
        outer = 5;
        inner = 5;
      };
      window = {
        border = 2;
        titlebar = false;
        commands = [
          {
            command = "opacity 0.95";
            criteria.app_id = "(${terminal})|(emacs)";
          }
          {
            command = "floating disable";
            criteria.title = "(Guitar Pro 8)|(DaVinci Resolve Studio)";
          }
          # https://github.com/flameshot-org/flameshot/issues/2970#issuecomment-1321117462
          {
            command = "floating enable, fullscreen disable, move absolute position 0 0, border pixel 0";
            criteria.app_id = "flameshot";
          }
        ];
      };
      startup = [
        {
          command = "systemctl --user import-environment";
          always = true;
        }
        {
          command = "swaysome init 1";
          always = true;
        }
        {command = "autotiling-rs";}
      ];
      input = {
        "*" = {
          xkb_layout = "pl";
          repeat_delay = "300";
          accel_profile = "flat";
        };
        touchpad = {
          dwt = "disabled";
          tap = "disabled";
        };
      };
      output = {
        "*" = with pkgs.rice; {
          bg = "${wallpaper} fill ${base00}";
        };
        "HDMI-A-2".pos = "1920 50";
        "DVI-D-1".pos = "0 0";
      };
      modifier = "Mod4";
      keybindings = let
        mod = config.wayland.windowManager.sway.config.modifier;
        inherit (config.wayland.windowManager.sway.config) up down left right terminal menu;
      in
        (pkgs.lib.mapAttrs'
          (n: _: pkgs.lib.nameValuePair "${mod}+${n}" "exec swaysome focus ${n}") (builtins.listToAttrs (map (n: {
            name = builtins.toString n;
            value = null;
          }) (pkgs.lib.range 1 9))))
        // (pkgs.lib.mapAttrs'
          (n: _: pkgs.lib.nameValuePair "${mod}+Shift+${n}" "exec swaysome move ${n}") (builtins.listToAttrs (map (n: {
            name = builtins.toString n;
            value = null;
          }) (pkgs.lib.range 1 9))))
        // {
          "${mod}+return" = "exec ${terminal}";
          "${mod}+space" = "exec ${menu}";
          "${mod}+Control+space" = "exec dunstctl close";
          "${mod}+backspace" = "exec wlr-which-key";
          "${mod}+Control+p" = "exec ${rofi-stuff}/bin/keepass";
          Print = "exec flameshot gui";
          "Control+Print" = "replay-sorcery save";

          "${mod}+w" = "kill";
          "${mod}+f" = "fullscreen toggle";
          "${mod}+r" = "mode resize";
          "${mod}+t" = "floating toggle";
          "${mod}+tab" = "layout toggle split tabbed";
          "${mod}+s" = "split toggle";
          "${mod}+Control+q" = "exit";
          "${mod}+Control+r" = "reload";

          "${mod}+${left}" = "focus left";
          "${mod}+${down}" = "focus down";
          "${mod}+${up}" = "focus up";
          "${mod}+${right}" = "focus right";

          "${mod}+Shift+${left}" = "move left";
          "${mod}+Shift+${down}" = "move down";
          "${mod}+Shift+${up}" = "move up";
          "${mod}+Shift+${right}" = "move right";

          "${mod}+Control+${left}" = "focus output left";
          "${mod}+Control+${down}" = "focus output down";
          "${mod}+Control+${up}" = "focus output up";
          "${mod}+Control+${right}" = "focus output right";

          "${mod}+Control+Shift+${left}" = "move container to output left";
          "${mod}+Control+Shift+${down}" = "move container to focus output down";
          "${mod}+Control+Shift+${up}" = "move container to focus output up";
          "${mod}+Control+Shift+${right}" = "move container to focus output right";

          XF86AudioMute = "exec amixer set Master toggle";
          XF86AudioRaiseVolume = "exec amixer set Master 5%+";
          XF86AudioLowerVolume = "exec amixer set Master 5%-";
          XF86AudioPlay = "exec playerctl -p strawberry play-pause";
          XF86AudioPrev = "exec playerctl -p strawberry previous";
          XF86AudioNext = "exec playerctl -p strawberry next";
        };
      modes = let
        inherit (config.wayland.windowManager.sway.config) up down left right;
      in {
        resize = {
          "${left}" = "resize shrink width 10 px";
          "${down}" = "resize grow height 10 px";
          "${up}" = "resize shrink height 10 px";
          "${right}" = "resize grow width 10 px";
          "Escape" = "mode default";
          "Return" = "mode default";
        };
      };
    };
    extraConfig = ''
      blur enable
      blur_xray enable
      shadows enable
      shadows_on_csd enable

      titlebar_separator disable
      titlebar_border_thickness 2
    '';
  }; # }}}

  wayland.windowManager.hyprland = {
    # {{{
    enable = true;
    systemd.variables = ["--all"];
    plugins = with pkgs.hyprlandPlugins; [
      split-monitor-workspaces
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
          resize_on_border = true;
        };
        decoration = {
          shadow_range = 8;
          shadow_render_power = 1;
          "col.shadow" = "0x80000000";
          blur = {
            new_optimizations = true;
            xray = true;
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
          vrr = 1;
          enable_swallow = true;
          swallow_regex = terminal;
        };
        master = {
          mfact = 0.5;
        };
        xwayland.force_zero_scaling = true;
        opengl.force_introspection = 1;
        monitor = [
          ", preferred, auto, 1"
          "HDMI-A-2, preferred, 1920x50, 1"
          "DVI-D-1, preferred, 0x0, 1"
        ];
        exec = [
          "swaybg -i ${wallpaper} -m fill"
          "systemctl --user restart waybar"
        ];
        bind =
          [
            "${mod}, return, exec, ${terminal}"
            "${mod}, space, exec, ${menu}"
            "${mod} CTRL, space, exec, dunstctl close"
            "${mod}, backspace, exec, wlr-which-key"
            "${mod} CTRL, p, exec, ${rofi-stuff}/bin/keepass"
            ", Print, exec, ${pkgs.writeShellScript "screenshot" "grimblast --freeze save area - | satty -f -"}"

            "${mod}, w, killactive"
            "${mod}, t, togglefloating"
            "${mod}, f, fullscreen"
            "${mod} SHIFT, f, fakefullscreen"
            "${mod} CTRL, q, exit"
            "${mod} CTRL, r, exec, hyprctl reload"
          ]
          ++ pkgs.lib.flatten (map (x: let
            x' = toString x;
          in [
            "${mod}, ${x'}, split-workspace, ${x'}"
            "${mod} SHIFT, ${x'}, split-movetoworkspacesilent, ${x'}"
          ]) (pkgs.lib.range 1 9));
        binde = [
          "${mod}, j, layoutmsg, cyclenext"
          "${mod}, k, layoutmsg, cycleprev"
          "${mod} SHIFT, j, layoutmsg, swapnext"
          "${mod} SHIFT, k, layoutmsg, swapprev"
          "${mod}, h, focusmonitor, -1"
          "${mod}, l, focusmonitor, +1"
          "${mod} SHIFT, h, split-changemonitorsilent, -1"
          "${mod} SHIFT, l, split-changemonitorsilent, +1"
          "${mod} CTRL, h, splitratio, -0.05"
          "${mod} CTRL, l, splitratio, +0.05"
          ", XF86AudioMute, exec, amixer set Master toggle"
          ", XF86AudioRaiseVolume, exec, amixer set Master '5%+'"
          ", XF86AudioLowerVolume, exec, amixer set Master '5%-'"
          ", XF86AudioPlay, exec, playerctl -p strawberry play-pause"
          ", XF86AudioPrev, exec, playerctl -p strawberry previous"
          ", XF86AudioNext, exec, playerctl -p strawberry next"
        ];
        bindm = [
          "${mod}, mouse:272, movewindow"
          "${mod}, mouse:273, resizewindow"
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
            "float, move onscreen 0 0, stayfocused, class:(flameshot)"
          ]
          ++ map (x: "opacity 0.95 0.85, class:(${x})") [
            terminal
            "emacs"
          ];
      };
  };
  # }}}

  programs.waybar = {
    # {{{
    enable = true;
    systemd = {
      enable = true;
      target = "hyprland-session.target";
    };
    settings = with pkgs.rice; let
      red = s: "<span foreground=\"${base08}\">${s}</span>";
      icon = i: "<span font=\"${monoFont} 11\">${i}<tt> </tt></span>";
      interval = 1;
    in {
      main = {
        layer = "top";
        spacing = 15;
        position = "top";
        height = 27;
        modules-left = [
          "hyprland/workspaces"
          "hyprland/window"
        ];
        modules-right = [
          "tray"
          (
            if config.hostname == "thonkpad"
            then "custom/network"
            else ""
          )
          "cpu"
          "memory"
          "temperature"
          (
            if config.hostname == "thonkpad"
            then "custom/battery"
            else ""
          )
          "wireplumber"
          "clock"
        ];
        "hyprland/window" = {
          max-length = 64;
          separate-outputs = true;
        };
        tray = {
          spacing = 15;
          reverse-direction = true;
        };
        "custom/network" = {
          exec = pkgs.writeShellScript "network" ''
            network () {
              WIFI=$(cat /sys/class/net/wlp3s0/carrier || echo 0)
              ETHERNET=$(cat /sys/class/net/enp0s25/carrier || echo 0)
              TEXT=$(iwgetid --raw || echo "")

              if [ $WIFI == 1 ] && [ $ETHERNET == 1 ]; then
                STATE=both
              elif [ $WIFI == 1 ] && [ $ETHERNET == 0 ]; then
                STATE=wifi
              elif [ $WIFI == 0 ] && [ $ETHERNET == 1 ]; then
                STATE=ethernet
                TEXT=""
              else
                STATE=none
                TEXT=""
              fi

              cat <<EOF
                {"text": "$TEXT", "alt": "$STATE"}
            EOF

            }

            network

            connmanctl monitor | while read -s line; do
              network
            done
          '';
          return-type = "json";
          on-click = "connman-gtk";
          format = "{icon} {}";
          format-icons = {
            both = icon "󰈁 ";
            wifi = icon "";
            ethernet = icon "󰈁";
            none = red (icon "󰈂");
          };
        };
        cpu = rec {
          inherit interval;
          states.critical = 95;
          tooltip = false;
          format = "${icon "󰘚"} {usage}%";
          format-critical = red format;
        };
        memory = rec {
          inherit interval;
          states.critical = 95;
          tooltip = false;
          format = "${icon "󰍛"} {used} GB";
          format-critical = red format;
        };
        temperature = rec {
          inherit interval;
          thermal-zone =
            if config.hostname == "machine-nixos"
            then 2
            else 0;
          states.critical = 80;
          tooltip = false;
          format = "${icon "󰔏"} {temperatureC}°C";
          format-critical = red format;
        };
        "custom/battery" = rec {
          exec = pkgs.writeShellScript "battery" ''
            battery () {
              case $(cat /sys/class/power_supply/AC/online) in
                1)
                  STATE=charging
                  ;;
                *)
                  STATE=discharging
                  ;;
              esac

              CAPACITY=$(cat /sys/class/power_supply/BAT0/capacity)
              if [ $CAPACITY -ge 98 ]; then
                CAPACITY=100
              fi

              cat <<EOF
                {"alt": "$STATE", "percentage": $CAPACITY}
            EOF
            }

            battery

            upower --monitor | while read -s line; do
              battery
            done
          '';
          return-type = "json";
          states.critical = 20;
          tooltip = false;
          format = "${icon "{icon}"} {percentage}%";
          format-critical = red format;
          format-icons = {
            discharging = ["󰂎" "󰁺" "󰁻" "󰁼" "󰁽" "󰁾" "󰁿" "󰂀" "󰂁" "󰂂" "󰁹"];
            charging = ["󰢜" "󰂆" "󰂇" "󰂈" "󰢝" "󰂉" "󰢞" "󰂊" "󰂋" "󰂅"];
          };
        };
        wireplumber = {
          tooltip = false;
          format = "${icon ""} {volume}%";
          format-muted = red (icon "");
          on-click = "amixer set Master toggle";
          on-click-right = "pwvucontrol";
        };
        clock = {
          inherit interval;
          format = "${icon ""} {:%A %d %B %H:%M:%S}";
        };
      };
    };
    style = pkgs.rice.compileSCSS ./waybar.scss;
  }; # }}}

  programs.foot = {
    enable = true;
    server.enable = true;
    settings = let
      color = pkgs.lib.removePrefix "#";
    in
      with pkgs.rice; {
        main = {
          shell = "zellij";
          font = "${monoFont}:size=11.5";
          pad = "5x5 center";
        };
        bell.urgent = "yes";
        url.osc8-underline = "always";
        cursor = {
          style = "beam";
          blink = "yes";
          color = "${color base00} ${color base0F}";
        };
        mouse.hide-when-typing = "yes";
        colors = {
          foreground = color base05;
          background = color base00;
          urls = color base0F;
          flash = color base08;

          regular0 = color base01;
          regular1 = color base08;
          regular2 = color base0B;
          regular3 = color base09;
          regular4 = color base0D;
          regular5 = color base0E;
          regular6 = color base0C;
          regular7 = color base06;

          bright0 = color base02;
          bright1 = color base12;
          bright2 = color base14;
          bright3 = color base13;
          bright4 = color base16;
          bright5 = color base17;
          bright6 = color base15;
          bright7 = color base0F;
        };
      };
  };

  programs.hyprlock = let
    color = c: "rgb(${pkgs.lib.removePrefix "#" c})";
  in
    with pkgs.rice; {
      enable = true;
      backgrounds = [{path = builtins.toString wallpaperBlurred;}];
      input-fields = [
        {
          bothlock_color = color base0E;
          capslock_color = color base0E;
          check_color = color base0A;
          dots_size = 0.2;
          fade_timeout = 300;
          fail_color = color base08;
          fail_text = "$FAIL";
          font_color = color base05;
          inner_color = color base02;
          numlock_color = color base0E;
          outer_color = color base02;
          outline_thickness = 2;
          placeholder_text = "";
          shadow_passes = 1;
        }
      ];
      labels = [
        {
          color = color base05;
          font_family = uiFont;
          shadow_passes = 1;
          text = "$TIME";
        }
      ];
    };

  services.hypridle = {
    enable = true;
    lockCmd = "pidof hyprlock || hyprlock";
    beforeSleepCmd = "loginctl lock-session";
    afterSleepCmd = "hyprctl dispatch dpms on";
    listeners = [
      {
        timeout = 300;
        onTimeout = "loginctl lock-session";
      }
      {
        timeout = 360;
        onTimeout = "hyprctl dispatch dpms off";
        onResume = "hyprctl dispatch dpms on";
      }
    ];
  };

  xdg.configFile = {
    "satty/config.toml".source = (pkgs.formats.toml {}).generate "satty.toml" {
      general = {
        copy-command = "wl-copy";
        save-after-copy = true;
        output-filename = "/home/bandithedoge/Pictures/%F_%H:%M:%S.png";
      };
      color-palette = with pkgs.rice; {
        first = base0F;
        second = base08;
        third = base0B;
        fourth = base09;
        fifth = base0D;
      };
    };

    "wlr-which-key/config.yaml".text = with pkgs.rice;
      builtins.toJSON {
        font = uiFont;
        background = base02;
        color = base05;
        border = base0F;
        separator = " 󰅂 ";
        border_width = 2;
        corner_r = 0;
        padding = 5;
        anchor = "bottom-right";
        margin_bottom = 5;
        margin_right = 5;
        menu =
          {
            d = {
              desc = "Discord";
              cmd = "vesktop";
            };
            p = {
              desc = "Music player";
              cmd = "strawberry";
            };
            b = {
              desc = "Web browser";
              cmd = "$BROWSER";
            };
            g = {
              desc = "Game launcher";
              cmd = "lutris";
            };
            k = {
              desc = "Password manager";
              cmd = "keepassxc";
            };
            e = {
              desc = "Emacs";
              cmd = "emacs";
            };
          }
          // pkgs.lib.optionalAttrs (config.hostname == "machine-nixos") {
            v = {
              desc = "Virtual machines";
              cmd = "virt-manager";
            };
            l = {
              desc = "Looking Glass Client";
              cmd = "looking-glass-client";
            };
          };
      };
  };
}
