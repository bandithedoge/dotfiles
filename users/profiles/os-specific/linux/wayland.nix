{pkgs, ...}: let
  rofi-stuff = pkgs.callPackage ./rofi {};
in {
  home.packages = with pkgs; [
    river
    wev
    wl-clipboard
  ];

  xdg.configFile = {
    "hypr/hyprpaper.conf".text = with pkgs.rice; ''
      ipc = off

      preload = ${wallpaper}
      wallpaper = , ${wallpaper}
    '';
    # river {{{
    "river/init".source = let
      mod = "Super";
      color = c: "0x${pkgs.lib.removePrefix "#" c}";
    in
      with pkgs.rice;
        pkgs.writeShellScript "river" ''
          riverctl map normal ${mod} Return spawn ${terminal}
          riverctl map normal ${mod} Space spawn "${menu}"
          riverctl map normal ${mod} B spawn $BROWSER
          riverctl map normal ${mod} P spawn strawberry
          riverctl map normal ${mod}+Control P spawn ${rofi-stuff}/bin/keepass
          riverctl map normal ${mod} D spawn discordcanary
          riverctl map normal None Print spawn ${pkgs.writeShellScript "screenshot" ''
            sel=$(${pkgs.slurp}/bin/slurp -d -b "${base00}AA" -c "${base0F}FF" -B "${base00}FF" -F "${uiFont}")
            ${pkgs.grim}/bin/grim -g "$sel" - | ${pkgs.swappy}/bin/swappy -f -
          ''}

          riverctl map normal ${mod} W close
          riverctl map normal ${mod}+Control Q exit
          riverctl map normal ${mod} T toggle-float
          riverctl map normal ${mod} F toggle-fullscreen

          riverctl map normal ${mod} J focus-view next
          riverctl map normal ${mod} K focus-view previous
          riverctl map normal ${mod}+Shift J swap next
          riverctl map normal ${mod}+Shift K swap previous
          riverctl map normal ${mod}+Control H send-layout-cmd rivertile "main-ratio -0.05"
          riverctl map normal ${mod}+Control L send-layout-cmd rivertile "main-ratio +0.05"
          riverctl map-pointer normal ${mod} BTN_LEFT move-view
          riverctl map-pointer normal ${mod} BTN_RIGHT resize-view

          riverctl map normal ${mod} H focus-output previous
          riverctl map normal ${mod} L focus-output next
          riverctl map normal ${mod}+Shift H send-to-output previous
          riverctl map normal ${mod}+Shift L send-to-output next

          for i in $(seq 1 9)
          do
            tags=$((1 << ($i - 1)))
            riverctl map normal ${mod} $i set-focused-tags $tags
            riverctl map normal ${mod}+Shift $i set-view-tags $tags
          done

          riverctl map normal None XF86AudioMute spawn "amixer set Master toggle"
          riverctl map normal None XF86AudioRaiseVolume spawn "amixer set Master 5%+"
          riverctl map normal None XF86AudioLowervolume spawn "amixer set Master 5%-"
          riverctl map normal None XF86AudioPlay spawn "playerctl play-pause"
          riverctl map normal None XF86AudioPrev spawn "playerctl previous"
          riverctl map normal None XF86AudioNext spawn "playerctl next"

          riverctl map-switch normal lid close spawn "${pkgs.waylock}/bin/waylock -init-color ${color base00} -input-color ${color base02} -fail-color ${color base08}"

          riverctl keyboard-layout pl
          riverctl input pointer-2-7-SynPS/2_Synaptics_TouchPad disable-while-typing disabled
          riverctl input pointer-2-7-SynPS/2_Synaptics_TouchPad natural-scroll enabled
          riverctl input pointer-2-7-SynPS/2_Synaptics_TouchPad scroll-method two-finger
          riverctl input pointer-2-7-SynPS/2_Synaptics_TouchPad tap disabled

          riverctl background-color ${color base00}
          riverctl border-color-focused ${color base0F}
          riverctl border-color-unfocused ${color base00}
          riverctl border-width 2
          riverctl focus-follows-cursor normal

          riverctl default-layout rivertile
          rivertile \
            -view-padding 5 \
            -outer-padding 5 \
            -main-ratio 0.5 \
            &

          ${pkgs.hyprpaper}/bin/hyprpaper &
          ${pkgs.waybar}/bin/waybar &

          exec dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=river
        '';
    # }}}
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
          "river/tags"
          "river/window"
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
