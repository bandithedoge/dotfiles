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
      satty
      swaybg
      swaysome
      wev
      wl-clipboard
      wlr-which-key
    ];
    sessionVariables = {
      GDK_BACKEND = "wayland,x11";
      QT_QPA_PLATFORM = "wayland;xcb";
      SDL_VIDEODRIVER = "wayland";
      _JAVA_AWT_WM_NONREPARENTING = "1";
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
    # {{{
    enable = true;
    # package = pkgs.sway_git;
    config = {
      bars = [];
      inherit (pkgs.rice) terminal menu;
      colors = with pkgs.rice; {
        background = base00;
        focused = {
          background = base0F;
          border = base0F;
          childBorder = base0F;
          indicator = base0F;
          text = base00;
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
            criteria.app_id = pkgs.rice.terminal;
          }
          {
            command = "floating disable";
            criteria.title = "(Guitar Pro 8)|(DaVinci Resolve Studio)";
          }
        ];
      };
      startup = [
        {
          command = "swaysome init 1";
          always = true;
        }
        {command = "autotiling-rs";}
        {
          command = "killall -r 'waybar*'; waybar";
          always = true;
        }
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
          Print = "exec ${pkgs.writeShellScript "screenshot" (with pkgs; ''
            ${lib.getExe wayshot} -o $(swaymsg -t get_outputs | ${lib.getExe jq} 'map(select(.focused)).[0].name' -r) --stdout | satty -f - --output-filename ~/Pictures/$(date "+%F-%T").png
          '')}";

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
      titlebar_border_thickness 0
    '';
  }; # }}}

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
          "sway/workspaces"
          "sway/window"
        ];
        modules-right = [
          "tray"
          # "mpris"
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
        "sway/window" = {
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
