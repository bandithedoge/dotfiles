{
  pkgs,
  config,
  ...
}: let
  rofi-stuff = pkgs.callPackage ../rofi {};
in {
  home = {
    packages = with pkgs; [
      autotiling
      chayang
      gtklock
      qtile_git
      satty
      sway-contrib.grimshot
      swaybg
      swaysome
      waylock
      wev
      wl-clipboard
      wlr-which-key
    ];
    sessionVariables = {
      GDK_BACKEND = "wayland,x11";
      NIXOS_OZONE_WL = "1";
      QT_QPA_PLATFORM = "wayland;xcb";
      _JAVA_AWT_WM_NONREPARENTING = "1";
    };
  };

  wayland.windowManager.sway = with pkgs.rice; {
    # {{{
    enable = true;
    package = pkgs.sway;
    # checkConfig = false; # swayfx fails to create a renderer when rebuilding
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
          {
            command = "inhibit_idle open";
            criteria.app_id = "virt-manager";
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
        {
          command = "autotiling";
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
        "DP-1".pos = "0 450";
        "DP-2".pos = "1920 500";
        "DP-3" = {
          transform = "270";
          pos = "3840 0";
        };
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
          Print = "exec 'grimshot save anything - | satty -f -'";
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

          XF86AudioMute = "exec wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
          XF86AudioRaiseVolume = "exec wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+";
          XF86AudioLowerVolume = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-";
          XF86AudioPlay = "exec mpc toggle";
          XF86AudioPrev = "exec mpc prev";
          XF86AudioNext = "exec mpc next";
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
    # extraConfig = ''
    #   titlebar_separator disable
    #   titlebar_border_thickness 2
    # '';
  }; # }}}

  programs.waybar = {
    # {{{
    # TODO: window title overflowing
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
          on-click-middle = "coppwr";
        };
        clock = {
          inherit interval;
          format = "${icon ""} {:%A %d %B %H:%M:%S}";
        };
      };
    };
    style = pkgs.rice.compileSCSS ./waybar.scss;
  }; # }}}

  services.hypridle = {
    enable = true;
    settings = {
      general = {
        lock_cmd = "pidof waylock || (chayang -d 10 && gtklock)";
        before_sleep_cmd = "loginctl lock-session";
      };
      listener = [
        {
          timeout = 300;
          on-timeout = "loginctl lock-session";
        }
        {
          timeout = 360;
          on-timeout = "swaymsg output '*' power off";
          on-resume = "swaymsg output '*' power on";
        }
      ];
    };
  };

  xdg.configFile = {
    "gtklock/config.ini".source = (pkgs.formats.ini {}).generate "gtklock.ini" {
      main = {
        gtk-theme = pkgs.rice.gtk.theme.name;
        modules = pkgs.lib.concatStringsSep ";" [
          "${pkgs.gtklock-playerctl-module}/lib/gtklock/playerctl-module.so"
          "${pkgs.gtklock-powerbar-module}/lib/gtklock/powerbar-module.so"
        ];
      };
      powerbar = {
        suspend-command =
          if config.hostname == "thonkpad"
          then "systemctl suspend"
          else "";
        show-labels = true;
        linked-buttons = true;
      };
    };

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
              cmd = "cantata";
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
            f = {
              desc = "Ferdium";
              cmd = "ferdium";
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
