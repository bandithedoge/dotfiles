{pkgs, ...}: let
  rofi-stuff = pkgs.callPackage ./rofi {};
in {
  home.packages = with pkgs; [
    wev
    wl-clipboard
  ];

  xdg.configFile = {
    "hypr/hyprpaper.conf".text = with pkgs.rice; ''
      ipc = off

      preload = ${wallpaper}
      wallpaper = , ${wallpaper}
    '';
  };

  wayland.windowManager.hyprland = {
    enable = true;
    extraConfig = let
      color = c: "0xFF${pkgs.lib.removePrefix "#" c}";
      mod = "SUPER";
    in
      with pkgs.rice; ''
        general {
          border_size = 2
          gaps_in = 5
          gaps_out = 10
          layout = master
          resize_on_border = true
          col.inactive_border = ${color base00}
          col.active_border = ${color base0F}
        }

        decoration {
          multisample_edges = false
          blur = false
        }

        input {
          kb_layout = pl
          scroll_method = 2fg
          touchpad {
            disable_while_typing = false
            natural_scroll = true
            tap-to-click = false
          }
        }

        misc {
          disable_hyprland_logo = true
          disable_splash_rendering = true
          mouse_move_enables_dpms = true
          key_press_enables_dpms = true
          disable_autoreload = true
          focus_on_activate = true
          animate_manual_resizes = false
          animate_mouse_windowdragging = false
        }

        animations {
          animation = windows, 1, 2, default, popin 80%
          animation = windowsOut, 0, 2, default
          animation = border, 1, 2, default
          animation = workspaces, 1, 2, default, fade
          animation = fade, 1, 2, default
        }

        master {
          new_on_top = true
          mfact = 0.5
        }

        ${builtins.concatStringsSep "\n" (map (class: "windowrulev2 = opacity 0.95 0.85, class:^(${class})$\n") [
          "kitty"
        ])}

        env = QT_QPA_PLATFORM, wayland;xcb
        env = SDL_VIDEODRIVER, wayland

        exec-once = ${pkgs.hyprpaper}/bin/hyprpaper
        exec-once = ${pkgs.waybar-hyprland}/bin/waybar

        bindm = ${mod}, mouse:272, movewindow
        bindm = ${mod}, mouse:273, resizewindow

        bind = ${mod}, Return, exec, ${terminal}
        bind = ${mod}, Space, exec, ${menu}
        bind = ${mod}, b, exec, $BROWSER
        bind = ${mod}, p, exec, strawberry
        bind = ${mod} CTRL, p, exec, ${rofi-stuff}/bin/keepass
        bind = ${mod}, d, exec, discordcanary
        bind = , Print, exec, ${pkgs.writeShellScript "screenshot" ''
          sel=$(${pkgs.slurp}/bin/slurp -d -b "${base00}AA" -c "${base0F}FF" -B "${base00}FF" -F "${uiFont}")
          ${pkgs.grim}/bin/grim -g "$sel" - | ${pkgs.swappy}/bin/swappy -f -
        ''}

        bind = , XF86AudioMute, exec, amixer set Master toggle
        bind = , XF86AudioRaiseVolume, exec, amixer set Master 5%+
        bind = , XF86AudioLowerVolume, exec, amixer set Master 5%-
        bind = , XF86AudioPlay, exec, playerctl play-pause
        bind = , XF86AudioPrev, exec, playerctl previous
        bind = , XF86AudioNext, exec, playerctl next

        binde = ${mod}, w, killactive
        bind = ${mod} CTRL, q, exit
        bind = ${mod}, t, togglefloating
        bind = ${mod}, f, fullscreen

        binde = ${mod}, j, layoutmsg, cyclenext
        binde = ${mod}, k, layoutmsg, cycleprev
        binde = ${mod} SHIFT, j, layoutmsg, swapnext
        binde = ${mod} SHIFT, k, layoutmsg, swapprev
        binde = ${mod} CTRL, h, resizeactive, -10 0
        binde = ${mod} CTRL, j, resizeactive, 0 10
        binde = ${mod} CTRL, k, resizeactive, 0 -10
        binde = ${mod} CTRL, l, resizeactive, 10 0

        binde = ${mod}, h, focusmonitor, -1
        binde = ${mod}, l, focusmonitor, +1
        binde = ${mod} SHIFT, h, movetoworkspace, m-1
        binde = ${mod} SHIFT, l, movetoworkspace, m+1

        ${builtins.concatStringsSep "\n" (map (x: ''
          bind = ${mod}, ${x}, workspace, ${x}
          bind = ${mod} SHIFT, ${x}, movetoworkspace, ${x}
        '') (map builtins.toString (pkgs.lib.range 1 9)))}
      '';
  };

  programs.waybar = {
    # {{{
    enable = true;
    settings = with pkgs.rice; let
      red = s: "<span foreground=\"${base08}\">${s}</span>";
      icon = i: "<span font=\"${monoFont} 11\">${i}<tt> </tt></span>";
      interval = 1;
    in {
      main = {
        spacing = 15;
        position = "top";
        height = 27;
        modules-left = [
          "wlr/workspaces"
          "hyprland/window"
        ];
        modules-right = [
          "tray"
          "custom/network"
          "cpu"
          "memory"
          "temperature"
          "custom/battery"
          "wireplumber"
          "clock"
        ];
        modules = {
          "hyprland/window" = {
            max-length = 64;
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
              both = icon " ";
              wifi = icon "";
              ethernet = icon "";
              none = red (icon "");
            };
          };
          cpu = rec {
            inherit interval;
            states.critical = 95;
            tooltip = false;
            format = "${icon "﬙"} {usage}%";
            format-critical = red format;
          };
          memory = rec {
            inherit interval;
            states.critical = 95;
            tooltip = false;
            format = "${icon ""} {used} GB";
            format-critical = red format;
          };
          temperature = rec {
            inherit interval;
            states.critical = 80;
            tooltip = false;
            format = "${icon ""} {temperatureC}°C";
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
              discharging = ["" "" "" "" "" "" "" "" "" "" ""];
              charging = ["" "" "" "" "" "" "" ""];
            };
          };
          wireplumber = {
            format = "${icon ""} {volume}%";
            format-muted = red (icon "");
            on-click = "amixer set Master toggle";
            on-click-right = "pavucontrol";
          };
          clock = {
            inherit interval;
            format = "${icon ""} {:%A %d %B %H:%M:%S}";
          };
        };
      };
    };
    style = pkgs.rice.compileSCSS ./waybar.scss;
  }; # }}}

  services.swayidle = let
    command = "${pkgs.gtklock}/bin/gtklock";
  in {
    enable = true;
    systemdTarget = "graphical-session.target";
    timeouts = [
      {
        timeout = 120;
        inherit command;
      }
    ];
    events = [
      {
        event = "before-sleep";
        inherit command;
      }
    ];
  };

  services.mako = with pkgs.rice; {
    enable = true;
    layer = "overlay";
    anchor = "bottom-right";
    font = "${uiFont} 12";
    backgroundColor = base02;
    textColor = base05;
    borderSize = 2;
    borderColor = base0F;
    progressColor = "over ${base0F}";
    margin = "5";
  };
}
