{pkgs, ...}: {
  home.packages = with pkgs; [
    eww
  ];

  xdg.configFile = {
    "eww/eww.yuck".text = ''
      (deflisten xmonad
        "${pkgs.haskellPackages.xmonad-dbus}/bin/xmonad-dbus")

      ${builtins.readFile ./eww.yuck}
    '';

    "eww/eww.scss".source = pkgs.rice.compileSCSS ./eww.scss;

    "eww/bin/network".source = pkgs.writeShellScript "network" ''
      network () {
        WIFI=$(cat /sys/class/net/wlp3s0/carrier || echo 0)
        ETHERNET=$(cat /sys/class/net/enp0s25/carrier || echo 0)
        SSID=$(iwgetid -r || echo "")

        cat <<EOF
          { "wifi": $([[ $WIFI == 1 ]] && echo "true" || echo "false"), "ethernet": $([[ $ETHERNET == 1 ]] && echo "true" || echo "false"), "ssid": "$SSID" }
      EOF
      }

      network

      connmanctl monitor | while read -s line; do
        network
      done
    '';

    "eww/bin/battery".source = pkgs.writeShellScript "battery" ''
      battery () {
        case $(cat /sys/class/power_supply/AC/online) in
          1) CHARGING=true ;;
          *) CHARGING=false ;;
        esac

        CAPACITY=$(cat /sys/class/power_supply/BAT0/capacity)
        if [ $CAPACITY -ge 98 ]; then
          CAPACITY=100
        fi

        cat <<EOF
          {"charging": $CHARGING, "percentage": $CAPACITY}
      EOF
      }

      battery

      upower --monitor | while read -s line; do
        battery
      done
    '';

    "eww/bin/volume".source = pkgs.writeShellScript "volume" ''
      volume() {
        VOLUME=$(amixer get Master | grep -Poe "\d+%" | head -n 1 | grep -Poe "\d+")
        MUTED=$(amixer get Master | grep -o "\[off\]" > /dev/null && echo "true" || echo "false")
        cat <<EOF
          {"muted": $MUTED, "percentage": $VOLUME}
      EOF
      }

      volume

      alsactl monitor | while read -s line; do
        volume
      done
    '';

    "eww/bin/tray".source = pkgs.writeShellScript "tray" ''
      killall trayer

      trayer \
        --align right \
        --edge top \
        --widthtype request \
        --height 25 &

      sleep 1

      xprop -spy -name panel | while read -s line; do
        PADDING=$(xprop -name panel | grep -Poe "program specified minimum size: \d+" | grep -Poe "\d+")
        echo $PADDING
      done
    '';
  };
}
