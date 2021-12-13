{ home-manager, pkgs, ... }:
let rice = import ../../rice.nix;
in {
  home.packages = with pkgs; [
    river
    wlr-randr
    yambar
    wayvnc
    swaylock
    swayidle
    wofi
    pavucontrol
    flat-remix-icon-theme
    vcv-rack
    milkytracker
  ];

  # river {{{
  xdg.configFile."river/init" =
    let colors = color: "0x" + pkgs.lib.strings.removePrefix "#" color;
    in {
      executable = true;
      text = ''
        #!/usr/bin/env bash

        mod="Mod4"

        riverctl map normal $mod Return spawn ${rice.terminal}
        riverctl map normal $mod Space spawn "${rice.menu}"
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

        riverctl background-color ${colors rice.bg0}
        riverctl border-color-focused ${colors rice.accent}
        riverctl border-color-unfocused ${colors rice.bg}

        riverctl default-layout rivertile
        exec yambar &
        exec rivertile -view-padding 5 -outer-padding 5 -main-ratio 0.5
      '';
    };
  # }}}

  # yambar {{{
  xdg.configFile."yambar/config.yml".text = let
    colors = color: pkgs.lib.strings.removePrefix "#" color + "ff";
    spacing = 5;
    module = text: {
      inherit text;
      margin = spacing;
      deco.background.color = colors rice.bg2;
    };
    icon = text:
      module text // {
        font = rice.monoFont + ":size=12";
        deco.background.color = colors rice.bg;
      };
    module_red = text: (module text) // { foreground = colors rice.red; };
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
      background = colors rice.bg;
      foreground = colors rice.fg;
      font = rice.uiFont + ":size=14";
      left = [
        {
          "river" = {
            content.map = let
              tagmap = {
                tag = "focused";
                values = {
                  true.string = {
                    text = "{id}";
                    deco.background.color = colors rice.accent0;
                    foreground = colors rice.accent1;
                    font = rice.monoFont;
                    margin = spacing;
                  };
                  false.string = {
                    text = "{id}";
                    deco.background.color = colors rice.bg2;
                    foreground = colors rice.comment;
                    font = rice.monoFont;
                    margin = spacing;
                  };
                };
              };
            in {
              tag = "occupied";
              values.false.map = {
                tag = "focused";
                values = {
                  true = tagmap.values.true;
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
                  {
                    string = (module "Disconnected") // {
                      foreground = colors rice.red;
                    };
                  }
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
                  {
                    string = (module "Disconnected") // {
                      foreground = colors rice.red;
                    };
                  }
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
                charging =
                  [ { string = icon ""; } { string = module "{capacity}%"; } ];
                discharging =
                  [ { string = icon ""; } { string = module "{capacity}%"; } ];
                unknown =
                  [ { string = icon ""; } { string = module "{capacity}%"; } ];
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
                false =
                  [ { string = icon "墳"; } { string = module "{percent}%"; } ];
                true =
                  [ { string = icon "婢"; } { string = module_red "Muted"; } ];
              };
            };
          };
        }
        {
          "clock" = {
            date-format = "%A %d %B";
            content = [ { string = icon ""; } { string = module "{date}"; } ];
          };
        }
        {
          "clock" = {
            time-format = "%H:%M:%S";
            content = [ { string = icon ""; } { string = module "{time}"; } ];
          };
        }
      ];
    };
  };
  # }}}

  programs = {
    qutebrowser = {
      enable = true;
      searchEngines = {
        DEFAULT = "https://searx.be/search?q={}";
        g = "https://www.google.com/search?q={}";
        a = "https://wiki.archlinux.org/?search={}";
        n = "https://nixos.wiki/index.php?search={}";
      };
      settings = {
        scrolling.smooth = true;
        fonts = {
          default_family = rice.uiFont;
          default_size = "14px";
        };
        colors = {
          completion = {
            inherit (rice) fg;
            even.bg = rice.bg1;
            match.fg = rice.accent;
            odd.bg = rice.bg;
            scrollbar = {
              inherit (rice) bg;
              fg = rice.accent;
            };
            item.selected = {
              border = {
                bottom = rice.accent0;
                top = rice.accent0;
              };
              bg = rice.accent0;
              fg = rice.accent1;
              match.fg = rice.bg;
            };
            category = {
              bg = rice.bg2;
              fg = rice.accent;
              border = {
                bottom = rice.bg2;
                top = rice.bg2;
              };
            };
          };
          contextmenu = {
            disabled = {
              bg = rice.bg2;
              fg = rice.comment;
            };
            menu = {
              bg = rice.bg2;
              inherit (rice) fg;
            };
            selected = {
              bg = rice.accent0;
              fg = rice.accent1;
            };
          };
          downloads = {
            bar.bg = rice.bg;
            error = {
              bg = rice.red0;
              fg = rice.red1;
            };
            start = {
              bg = rice.bg2;
              inherit (rice) fg;
            };
            stop = {
              bg = rice.green0;
              fg = rice.green1;
            };
            system = {
              bg = "none";
              fg = "none";
            };
          };
          hints = {
            bg = rice.bg2;
            inherit (rice) fg;
            match.fg = rice.accent;
          };
        };
      };
    };
  };

  services = { syncthing.enable = true; };
}
