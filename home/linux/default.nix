{ home-manager, pkgs, ... }:
let colors = import ../../rice.nix;
in {

  # river {{{
  xdg.configFile."river/init" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      mod="Mod1"

      riverctl map normal $mod Return spawn foot
      riverctl map normal $mod W close
      riverctl map normal $mod+Control Q exit

      riverctl map normal $mod J focus-view next
      riverctl map normal $mod K focus-view previous
      riverctl map normal $mod+Shift J swap next
      riverctl map normal $mod+Shift K swap previous

      for i in $(seq 1 9)
      do
        tags=$((1 << ($i - 1)))

        riverctl map normal $mod $i set-focused-tags $tags
        riverctl map normal $mod+Shift $i set-view-tags $tags
        riverctl map normal $mod+Control $i toggle-focused-tags $tags
        riverctl map normal $mod+Shift+Control $i toggle-view-tags $tags
      done

      riverctl map normal $mod H send-layout-cmd rivertile "main-ratio -0.03"
      riverctl map normal $mod L send-layout-cmd rivertile "main-ratio +0.03"
      riverctl map normal $mod+Shift H send-layout-cmd rivertile "main-count +1"
      riverctl map normal $mod+Shift L send-layout-cmd rivertile "main-count -1"

      riverctl map normal $mod+Control H send-layout-cmd rivertile "main-location left"
      riverctl map normal $mod+Control J send-layout-cmd rivertile "main-location bottom"
      riverctl map normal $mod+Control K send-layout-cmd rivertile "main-location top"
      riverctl map normal $mod+Control L send-layout-cmd rivertile "main-location right"

      riverctl map-pointer normal $mod BTN_LEFT move-view
      riverctl map-pointer normal $mod BTN_RIGHT resize-view

      riverctl map normal $mod F toggle-fullscreen
      riverctl map normal $mod T toggle-float

      riverctl float-filter-add app-id float

      riverctl background-color ${
        "0x" + pkgs.lib.strings.removePrefix "#" colors.bg
      }
      riverctl border-color-focused ${
        "0x" + pkgs.lib.strings.removePrefix "#" colors.accent
      }
      riverctl border-color-unfocused ${
        "0x" + pkgs.lib.strings.removePrefix "#" colors.bg
      }

      riverctl default-layout rivertile

      exec rivertile -view-padding 5 -outer-padding 5 -main-ratio 0.5 &

      waybar
    '';
  };
  # }}}

  # yambar {{{
  xdg.configFile."yambar/config.yml".text = ''
    bar:
      location: top
      height: 25
  '';
  # }}}

  # sway {{{
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    config = {
      modifier = "Mod4";
      menu = "${pkgs.wofi}/bin/wofi";
      terminal = "${pkgs.foot}/bin/foot";
      fonts = { };

      input."*" = {
        xkb_layout = "pl";
        xkb_options = "caps:ctrl_modifier";
      };

      gaps = {
        inner = 5;
        outer = 5;
      };

      colors = with colors; {
        background = bg;
        focused = {
          background = accent0;
          text = accent1;
          border = accent0;
          indicator = accent;
          childBorder = accent;
        };
        focusedInactive = {
          background = bg2;
          text = fg;
          border = fg;
          indicator = fg;
          childBorder = fg2;
        };
        unfocused = {
          background = bg;
          text = fg;
          border = bg2;
          indicator = bg2;
          childBorder = bg;
        };
        urgent = {
          background = red0;
          text = red1;
          border = red0;
          indicator = red;
          childBorder = red;
        };
      };

      /* keybindings = {
           "${modifier}+w" = "kill";
           "${modifier}+Return" = "exec ${terminal}";

           "${modifier}+Control+q" = "exec swaysmg exit";
           "${modifier}+Control+r" = "reload";

           "${modifier}+${left}" = "focus left";
           "${modifier}+${down}" = "focus down";
           "${modifier}+${up}" = "focus up";
           "${modifier}+${right}" = "focus right";

           "${modifier}+Shift+${left}" = "move left";
           "${modifier}+Shift+${down}" = "move down";
           "${modifier}+Shift+${up}" = "move up";
           "${modifier}+Shift+${right}" = "move right";

           "${modifier}+r" = "mode resize";
           "${modifier}+f" = "fullscreen toggle";
           "${modifier}+t" = "layout tabbed";
           "${modifier}+e" = "layout toggle split";

           "${modifier}+1" = "workspace number 1";
           "${modifier}+2" = "workspace number 2";
           "${modifier}+3" = "workspace number 3";
           "${modifier}+4" = "workspace number 4";
           "${modifier}+5" = "workspace number 5";
           "${modifier}+6" = "workspace number 6";
           "${modifier}+7" = "workspace number 7";
           "${modifier}+8" = "workspace number 8";
           "${modifier}+9" = "workspace number 9";

           "${modifier}+Shift+1" = "move container to workspace number 1";
           "${modifier}+Shift+2" = "move container to workspace number 2";
           "${modifier}+Shift+3" = "move container to workspace number 3";
           "${modifier}+Shift+4" = "move container to workspace number 4";
           "${modifier}+Shift+5" = "move container to workspace number 5";
           "${modifier}+Shift+6" = "move container to workspace number 6";
           "${modifier}+Shift+7" = "move container to workspace number 7";
           "${modifier}+Shift+8" = "move container to workspace number 8";
           "${modifier}+Shift+9" = "move container to workspace number 9";
         };
      */
    };

  };
  # }}}

  home.packages = with pkgs; [
    river
    wlr-randr
    yambar
    wayvnc
    swaylock
    swayidle
    milkytracker
  ];

  programs = {
    qutebrowser.enable = true;

    # waybar {{{
    waybar = {
      enable = true;
      systemd.enable = false;
      settings = [{
        layer = "top";
        position = "top";
        modules-left =
          [ "sway/mode" "sway/workspaces" "sway/window" "river/tags" ];
        modules-right = [
          "network"
          "battery"
          "temperature"
          "memory"
          "disk"
          "cpu"
          "pulseaudio"
          "clock#date"
          "clock#time"
        ];
        modules = {
          "clock#date".format = "  {:%A %d %B}";
          "clock#time" = {
            format = "  {:%T}";
            interval = 1;
          };
          "cpu".format = "  {usage}%";
          "battery" = {
            format = "{icon}  {capacity}%";
            format-charging = "ﮣ {icon}  {capacity}%";
            format-icons = [ "" "" "" "" "" "" "" "" "" "" "" ];
            states = { "low" = 20; };
          };
          "disk".format = "  {free}";
          "memory".format = "  {used:0.1} GiB";
          "network" = {
            format-wifi =
              "直  {essid} 祝 {bandwidthUpBits}  {bandwidthDownBits}";
            format-ethernet = "   祝 {bandwidthUpBits}  {bandwidthDownBits}";
            format-disconnected = "";
          };
          "pulseaudio" = {
            format = "{icon}  {volume}%";
            format-icons = { "speaker" = [ "奄" "奔" "墳" ]; };
            format-muted = "ﱝ";
            on-click = "amixer set Master toggle";
          };
          "temperature" = {
            critical-threshold = 70;
            format = "{icon}  {temperatureC}°C";
            format-icons = [ "" "" "" "" "" ];
          };
        };
      }];
      style = ''
        * {
          padding: 0;
          font-family: Roboto Condensed, FiraCode Nerd Font;
          font-size: 13px;
        }

        window#waybar {
          background-color: ${colors.bg};
          color: ${colors.fg};
        }

        .modules-right {
          margin: 0;
        }

        #clock, #cpu, #battery, #disk, #memory, #network, #pulseaudio, #temperature {
          background-color: ${colors.bg2};
          padding: 0 5px;
          margin-right: 10px;
        }

        #pulseaudio.muted, #network.disconnected, #battery.low, #temperature.critical {
          color: ${colors.red};
        }

        #window {
          margin-left: 10px;
        }

        #workspaces {
          margin: 0;
        }

        #workspaces button {
          background: ${colors.bg2};
          border-radius: 0;
          color: ${colors.fg};
        }

        #workspaces button.focused, #mode {
          background: ${colors.accent0};
          color: ${colors.accent1};
        }

        #mode {
          padding: 0 5px;
        }
      '';
    };
    # }}}

    # foot {{{
    foot = {
      enable = true;
      settings = {
        main = {
          font = "FiraCode Nerd Font:size=10";
          pad = "10x10center";
        };
        cursor = {
          style = "beam";
          blink = "yes";
        };
        mouse.hide-when-typing = "yes";
        colors = (builtins.mapAttrs
          (key: color: pkgs.lib.strings.removePrefix "#" color) (with colors; {
            foreground = fg;
            background = bg;
            selection-foreground = fg0;
            selection-background = selection;
            urls = accent;

            regular0 = bg0;
            regular1 = red;
            regular2 = green;
            regular3 = yellow;
            regular4 = blue;
            regular5 = purple;
            regular6 = cyan;
            regular7 = fg;

            bright0 = comment;
            bright1 = red1;
            bright2 = green1;
            bright3 = yellow1;
            bright4 = blue1;
            bright5 = purple1;
            bright6 = cyan1;
            bright7 = accent;
          }));
      };
    };
    # }}}
  };
}
