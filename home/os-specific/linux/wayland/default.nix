{
  pkgs,
  config,
  ...
}:
{
  imports = [
    ./niri.nix
  ];

  home = {
    packages = with pkgs; [
      chayang
      gtklock
      qt5.qtwayland
      qt6.qtwayland
      satty
      swaybg
      wev
      wl-clipboard
      wlr-randr
      wlr-which-key
    ];
    sessionVariables = {
      GDK_BACKEND = "wayland,x11,*";
      NIXOS_OZONE_WL = "1";
      QT_QPA_PLATFORM = "wayland;xcb";
      _JAVA_AWT_WM_NONREPARENTING = "1";
    };
  };

  programs = {
    waybar = {
      # {{{
      enable = true;
      style = pkgs.rice.compileSCSS "waybar.css" ./waybar.scss;
      systemd = {
        # enable = true;
        # target = "sway-session.target";
      };
      settings =
        with pkgs.rice;
        let
          red = s: "<span foreground=\"${base08}\">${s}</span>";
          icon = i: "<span font=\"${monoFont} 11\">${i}<tt> </tt></span>";
          interval = 1;
        in
        {
          main = {
            layer = "top";
            spacing = 15;
            position = "top";
            height = 27;
            modules-left = [
              "niri/workspaces"
              "niri/window"
            ];
            modules-right = [
              "tray"
              "privacy"
              # "mpris"
              "mpd"
              "gamemode"
              (if config.hostname == "thonkpad" then "custom/network" else "")
              "cpu"
              "memory"
              "temperature"
              (if config.hostname == "thonkpad" then "custom/battery" else "")
              "wireplumber"
              "clock"
            ];
            "sway/window" = {
              max-length = 64;
              separate-outputs = true;
            };
            "hyprland/workspaces" = {
              on-scroll-up = "hyprctl dispatch workspace m+1";
              on-scroll-down = "hyprctl dispatch workspace m-1";
            };
            "hyprland/window" = {
              icon = true;
              icon-size = 16;
              separate-outputs = true;
            };
            "niri/workspaces" = {
              on-scroll-up = "niri msg action focus-workspace-up";
              on-scroll-down = "niri msg action focus-workspace-down";
            };
            "niri/window" = {
              icon = true;
              icon-size = 16;
              separate-outputs = true;
            };
            tray = {
              spacing = 15;
              reverse-direction = true;
            };
            privacy = {
              icon-size = 16;
              modules = [ { type = "screenshare"; } ];
            };
            mpris = {
              format = "${icon "{status_icon}"} {artist} - {title}";
              player = "strawberry";
              status-icons = {
                playing = "󰐊";
                paused = "󰏤";
                stopped = "󰓛";
              };
              on-scroll-up = "playerctl -p strawberry volume 0.01+";
              on-scroll-down = "playerctl -p strawberry volume 0.01-";
            };
            mpd = {
              format = "${icon "{stateIcon}"} {artist} – {title}";
              on-click = "mpc toggle";
              on-click-right = "mpc next";
              on-click-middle = "mpc prev";
              on-scroll-up = "mpc volume +5";
              on-scroll-down = "mpc volume -5";
              state-icons = {
                playing = "󰐊";
                paused = "󰏤";
              };
            };
            gamemode = {
              use-icon = false;
              format = "${icon "󰺵"} {count}";
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
              thermal-zone = if config.hostname == "machine-nixos" then 2 else 0;
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
                discharging = [
                  "󰂎"
                  "󰁺"
                  "󰁻"
                  "󰁼"
                  "󰁽"
                  "󰁾"
                  "󰁿"
                  "󰂀"
                  "󰂁"
                  "󰂂"
                  "󰁹"
                ];
                charging = [
                  "󰢜"
                  "󰂆"
                  "󰂇"
                  "󰂈"
                  "󰢝"
                  "󰂉"
                  "󰢞"
                  "󰂊"
                  "󰂋"
                  "󰂅"
                ];
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
    }; # }}}

    swayimg = {
      enable = true;
      settings = with pkgs.rice; {
        viewer = {
          window = base00;
        };
        gallery = {
          window = base00;
          background = base02;
          select = base0F;
          border = base0F;
          shadow = base10;
        };
        font = {
          name = uiFont;
          size = 11;
          color = base05;
          shadow = base10;
          background = base00;
        };
        "info.viewer" = {
          top_left = "name,format,filesize,imagesize";
          top_right = "index,frame";
          bottom_left = "scale";
          bottom_right = "status";
        };
        "keys.viewer" = {
          "Shift+question" = "help";
          Tab = "mode";
          "Shift+j" = "next_file";
          "Shift+k" = "prev_file";
          h = "step_left 10";
          j = "step_down 10";
          k = "step_up 10";
          l = "step_right 10";
          ScrollUp = "zoom +10";
          ScrollDown = "zoom -10";
          "Shift+ScrollUp" = "step_left 10";
          "Shift+ScrollDown" = "step_right 10";
          "Ctrl+ScrollUp" = "step_up 10";
          "Ctrl+ScrollDown" = "step_down 10";
        };
        "keys.gallery" = {
          "Shift+question" = "help";
          Tab = "mode";
          h = "step_left";
          j = "step_down";
          k = "step_up";
          l = "step_right";
          g = "first_file";
          "Shift+g" = "last_file";
        };
      };
    };
  };

  services.swayidle = {
    enable = true;
    timeouts = [
      {
        timeout = 300;
        command = "chayang -d 10 && loginctl lock-session";
      }
    ];
    events = [
      {
        event = "lock";
        command = "pidof gtklock || ${with pkgs; lib.getExe gtklock}";
      }
    ];
  };

  services.mako = with pkgs.rice; {
    enable = true;
    settings = {
      max-visible = 10;
      layer = "overlay";
      font = "${uiFont} 11";
      background-color = base02;
      text-color = base05;
      margin = "5,5";
      border-size = 2;
      border-color = base0F;
      progress-color = base0F;
      default-timeout = 10000;

      on-button-left = "dismiss";
      on-button-right = "invoke-default-action";
      on-button-middle = "dismiss-all";

      "urgency=critical".border-color = base08;
      "mode=do-not-disturb".invisible = 1;
    };
  };

  xdg.configFile = {
    "gtklock/config.ini".source = (pkgs.formats.ini { }).generate "gtklock.ini" {
      main = {
        gtk-theme = pkgs.rice.gtk.theme.name;
        modules = pkgs.lib.concatStringsSep ";" [
          "${pkgs.gtklock-playerctl-module}/lib/gtklock/playerctl-module.so"
          "${pkgs.gtklock-powerbar-module}/lib/gtklock/powerbar-module.so"
        ];
      };
      powerbar = {
        suspend-command = if config.hostname == "thonkpad" then "systemctl suspend" else "";
        show-labels = true;
        linked-buttons = true;
      };
    };

    "satty/config.toml".source = (pkgs.formats.toml { }).generate "satty.toml" {
      general = {
        copy-command = "wl-copy";
        save-after-copy = true;
        output-filename = "/home/bandithedoge/Pictures/%F_%H:%M:%S.png";
      };
      font.family = pkgs.rice.uiFont;
      color-palette.palette = with pkgs.rice; [
        base08
        base09
        base0A
        base0B
        base0C
        base0D
        base0E
      ];
    };

    "wlr-which-key/config.yaml".text =
      with pkgs.rice;
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
          [
            {
              key = "d";
              desc = "Discord";
              cmd = "equibop";
            }
            {
              key = "b";
              desc = "Web browser";
              cmd = "$BROWSER";
            }
            {
              key = "g";
              desc = "Game launcher";
              cmd = "lutris";
            }
            {
              key = "k";
              desc = "Password manager";
              cmd = "secrets";
            }
            {
              key = "e";
              desc = "Emacs";
              cmd = "emacs";
            }
            {
              key = "c";
              desc = "Messenger";
              cmd = "caprine";
            }
            {
              key = "f";
              desc = "Matrix";
              cmd = "fractal";
            }
          ]
          ++ pkgs.lib.optionals (config.hostname == "machine-nixos") [
            {
              key = "p";
              desc = "Music player";
              cmd = "cantata";
            }
            {
              key = "v";
              desc = "Virtual machines";
              cmd = "virt-manager";
            }
            {
              key = "l";
              desc = "Looking Glass Client";
              cmd = "looking-glass-client";
            }
          ];
      };
  };
}
