{
  pkgs,
  config,
  ...
}: let
  rofi-stuff = pkgs.callPackage ../rofi {};
in {
  home = {
    packages = with pkgs; [
      swaybg
      wev
      wl-clipboard
      wlr-which-key
      satty
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
    recommendedEnvironment = true;
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
        "foot"
      ])}

      bind = , Print, exec, ${pkgs.writeShellScript "screenshot" (with pkgs; ''
        ${lib.getExe wayshot} -o $(hyprctl activeworkspace -j | ${lib.getExe jq} .monitor -r) --stdout | satty -f -
      '')}

      bind = $mod CTRL, p, exec, ${rofi-stuff}/bin/keepass
    '';
  };
  # }}}

  wayland.windowManager.sway = {
    enable = true;
    package = pkgs.sway_git;
    config = {
      bars = [];
      colors = with pkgs.rice; {
        background = base00;
        focused = {
          background = base0F;
          border = base0F;
          childBorder = base0F;
          indicator = base0F;
          text = base00;
        };
      };
    };
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
          "mpris"
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
        mpris = {
          player = "strawberry";
          format = "${icon "{status_icon}"}{artist} - {title}";
          status-icons = {
            playing = "󰐊";
            paused = "󰏤";
            stopped = "󰓛";
          };
          on-scroll-up = "playerctl volume 0.05+";
          on-scroll-down = "playerctl volume 0.05-";
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

  programs.foot = {
    enable = true;
    settings = let
      color = pkgs.lib.removePrefix "#";
    in
      with pkgs.rice; {
        main = {
          font = "${monoFont}:size=12";
          letter-spacing = "-1px";
          pad = "5x5 center";
        };
        bell = {
          urgent = "yes";
          notify = "yes";
          visual = "yes";
        };
        cursor = {
          style = "beam";
          blink = "yes";
          color = "${color base00} ${color base0F}";
        };
        mouse.hide-when-typing = "yes";
        colors = {
          foreground = color base05;
          background = color base00;
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
          selection-background = color base05;
          selection-foreground = color base00;
          flash = color base0F;
        };
      };
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
          cmd = "vencorddesktop";
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
      };
    };
}
