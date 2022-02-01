{ config, pkgs, ... }:
let rice = import ../../rice.nix;
in {
  imports = [ ./audio.nix ];
  home.packages = with pkgs; [
    icon-library
    imv
    pavucontrol
    river
    swayidle
    swaylock
    wl-clipboard
    wlr-randr
    yambar
  ];

  # river {{{
  xdg.configFile."river/init" =
    let colors = color: "0x" + pkgs.lib.strings.removePrefix "#" color;
    in {
      executable = true;
      text = with rice; ''
        #!/usr/bin/env bash

        mod="Mod4"

        riverctl map normal $mod Return spawn ${terminal}
        riverctl map normal $mod Space spawn "${menu}"
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

        riverctl background-color ${colors base00}
        riverctl border-color-focused ${colors base0F}
        riverctl border-color-unfocused ${colors base00}

        riverctl default-layout rivertile
        exec yambar &
        exec rivertile -view-padding 5 -outer-padding 5 -main-ratio 0.5
      '';
    };
  # }}}

  # yambar {{{
  xdg.configFile."yambar/config.yml".text = with rice;
    let
      color = hex: pkgs.lib.strings.removePrefix "#" hex + "ff";
      spacing = 5;
      module = text: {
        inherit text;
        margin = spacing;
        deco.background.color = color base02;
      };
      icon = text:
        module text // {
          font = rice.monoFont + ":size=12";
          deco.background.color = color base01;
        };
      module_red = text: (module text) // { foreground = color base08; };
      bitrate = [
        { string = icon ""; }
        { string = module "{rx-bitrate} Mb/s"; }
        { string = icon "祝"; }
        { string = module "{tx-bitrate} Mb/s"; }
      ];
    in builtins.toJSON {
      bar = {
        location = "top";
        height = 24;
        inherit spacing;
        background = color base01;
        foreground = color base05;
        font = uiFont + ":size=14";
        left = [
          {
            "river" = {
              content.map = let
                tagmap = {
                  tag = "focused";
                  values = {
                    true.string = {
                      text = "{id}";
                      deco.background.color = color base0F;
                      foreground = color base00;
                      font = monoFont;
                      margin = spacing;
                    };
                    false.string = {
                      text = "{id}";
                      deco.background.color = color base02;
                      foreground = color base03;
                      font = monoFont;
                      margin = spacing;
                    };
                  };
                };
              in {
                tag = "occupied";
                values.false.map = {
                  tag = "focused";
                  values = {
                    inherit (tagmap.values) true;
                    false.empty = { };
                  };
                };
                values.true.map = tagmap;
              };
            };
          }
          {
            "river" = {
              content.empty = { };
              title.string.text = "{title}";
            };
          }
        ];
        right = [
          {
            "network" = {
              name = "wlp3s0";
              content.map = {
                tag = "state";
                values = {
                  down = [
                    { string = icon "睊"; }
                    { string = module_red "Disconnected"; }
                  ];
                  up = bitrate
                    ++ [ { string = icon "直"; } { string = module "{ssid}"; } ];
                };
              };
            };
          }
          {
            "network" = {
              name = "enp0s25";
              content.map = {
                tag = "state";
                values = {
                  down = [
                    { string = icon ""; }
                    { string = module_red "Disconnected"; }
                  ];
                  up = [{ string = icon ""; }] ++ bitrate;
                };
              };
            };
          }
          {
            "battery" = {
              name = "BAT0";
              content.map = {
                tag = "state";
                values = {
                  full = [ { string = icon ""; } { string = module "Full"; } ];
                  charging = [
                    { string = icon ""; }
                    { string = module "{capacity}%"; }
                  ];
                  discharging = [
                    { string = icon ""; }
                    { string = module "{capacity}%"; }
                  ];
                  unknown = [
                    { string = icon ""; }
                    { string = module "{capacity}%"; }
                  ];
                };
              };
            };
          }
          {
            "alsa" = {
              card = "default";
              mixer = "Master";
              content.map = {
                on-click = "${pkgs.pavucontrol}/bin/pavucontrol";
                tag = "muted";
                values = {
                  false = [
                    { string = icon "墳"; }
                    { string = module "{percent}%"; }
                  ];
                  true =
                    [ { string = icon "婢"; } { string = module_red "Muted"; } ];
                };
              };
            };
          }
          {
            "clock" = {
              date-format = "%A %d %B";
              content =
                [ { string = icon ""; } { string = module "{date}"; } ];
            };
          }
          {
            "clock" = {
              time-format = "%H:%M:%S";
              content =
                [ { string = icon ""; } { string = module "{time}"; } ];
            };
          }
        ];
      };
    };
  # }}}

  gtk = {
    # {{{
    enable = true;
    font = {
      name = rice.uiFont;
      size = 12;
    };
    iconTheme = {
      package = pkgs.numix-icon-theme;
      name = "Numix";
    };
    theme = {
      package = pkgs.materia-theme.override {

      };
      name = "Materia-dark";
    };
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-enable-primary-paste = false;
    };
  }; # }}}

  programs.rofi = {
    # {{{
    enable = true;
    package = pkgs.nur.repos.kira-bruneau.rofi-wayland;
    font = rice.uiFont + " 12";
    extraConfig = {
      modi = "power:${./bin/rofi/power.lua},drun,run";
      drun-match-fields = "name,exec";
      drun-display-format = "{name}";
      show-icons = true;
      scroll-method = 1;
      kb-row-up = "Control+k";
      kb-row-down = "Control+j";
      kb-mode-next = "Control+l";
      kb-mode-previous = "Control+h";
      kb-mode-complete = "";
      kb-remove-to-eol = "";
      kb-accept-entry = "Return";
      kb-remove-char-back = "BackSpace";
    };
    theme = with rice;
      let
        inherit (config.lib.formats.rasi) mkLiteral;
        padding = mkLiteral "5px";
      in {
        "*" = {
          border-color = mkLiteral base0F;
          background-color = mkLiteral base00;
          text-color = mkLiteral base05;
        };

        mainbox.children =
          map mkLiteral [ "inputbar" "message" "mode-switcher" "listview" ];

        window = {
          border = mkLiteral "2px";
          anchor = mkLiteral "north";
        };

        entry = { inherit padding; };

        prompt = {
          inherit padding;
          background-color = mkLiteral base0F;
          text-color = mkLiteral base00;
        };

        listview.scrollbar = true;

        scrollbar.handle-color = mkLiteral base0F;

        element = {
          background-color = mkLiteral "transparent";
          padding = mkLiteral "2px 5px";
        };
        element-icon = {
          background-color = mkLiteral "inherit";
          padding = mkLiteral "0 5px";
        };
        "element.selected.normal".background-color = mkLiteral base02;
        element-text = {
          background-color = mkLiteral "inherit";
          highlight = mkLiteral "bold ${base0F}";
        };

        "element.urgent".text-color = mkLiteral base08;
        "element.active".text-color = mkLiteral base0B;
        "element.selected.urgent".background-color = mkLiteral base08;
        "element.selected.active".background-color = mkLiteral base0B;

        button.text-color = mkLiteral base03;
        "button.selected".text-color = mkLiteral base05;
      };
  }; # }}}

  programs.mako = {
    # {{{
    enable = true;
  }; # }}}

  services = { syncthing.enable = true; };
}
