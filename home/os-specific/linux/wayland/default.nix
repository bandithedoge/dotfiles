{pkgs, ...}: let
  rofi-stuff = pkgs.callPackage ../rofi {};
in {
  home = {
    packages = with pkgs; [
      swaybg
      wev
      wl-clipboard
      wlr-which-key
    ];
    sessionVariables = {
      GDK_BACKEND = "wayland,x11";
      QT_QPA_PLATFORM = "wayland;xcb";
      SDL_VIDEODRIVER = "wayland";
    };
  };

  wayland.windowManager.hyprland = {
    # {{{
    enable = true;
    plugins = with pkgs.hyprlandPlugins; [
      split-monitor-workspaces
    ];
    extraConfig = ''
      ${pkgs.rice.def.hypr}

      ${builtins.readFile ./hyprland.conf}

      ${builtins.concatStringsSep "\n" (map (x: let
        x' = toString x;
      in ''
        bind = $mod, ${x'}, split-workspace, ${x'}
        bind = $mod SHIFT, ${x'}, split-movetoworkspacesilent, ${x'}
      '') (pkgs.lib.range 1 9))}

      ${builtins.concatStringsSep "\n" (map (x: "windowrulev2 = opacity 0.95 0.85, class:(${x})") [
        "kitty"
      ])}

      bind = , Print, exec, ${pkgs.writeShellScript "screenshot" (with pkgs.rice; ''
        sel=$(${pkgs.slurp}/bin/slurp -d -b "${base00}AA" -c "${base0F}FF" -B "${base00}FF" -F "${uiFont}")
        ${pkgs.grim}/bin/grim -g "$sel" - | ${pkgs.swappy}/bin/swappy -f -
      '')}

      bind = $mod CTRL, p, exec, ${rofi-stuff}/bin/keepass
    '';
  };
  # }}}

  programs.waybar = {
    # {{{
    enable = true;
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
          "custom/network"
          "cpu"
          "memory"
          "temperature"
          "custom/battery"
          "wireplumber"
          "clock"
        ];
        "hyprland/window" = {
          max-length = 64;
          separate-outputs = true;
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
          on-click-right = "pavucontrol";
        };
        clock = {
          inherit interval;
          format = "${icon ""} {:%A %d %B %H:%M:%S}";
        };
      };
    };
    style = pkgs.rice.compileSCSS ./waybar.scss;
  }; # }}}

  services.swayidle = let
    command = "${pkgs.chayang}/bin/chayang -d 10 && ${pkgs.lib.getExe pkgs.gtklock}";
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

  xdg.configFile."wlr-which-key/config.yaml".text = with pkgs.rice;
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
      menu = {
        d = {
          desc = "Discord";
          cmd = "discordcanary";
        };
        p = {
          desc = "Music player";
          cmd = "strawberry";
        };
        b = {
          desc = "Web browser";
          cmd = "qutebrowser";
        };
        g = {
          desc = "Game launcher";
          cmd = "lutris";
        };
      };
    };
}
