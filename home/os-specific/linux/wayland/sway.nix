{pkgs, config, ...}: let
  rofi-stuff = pkgs.callPackage ../rofi {};
in {
  home.packages = with pkgs; [
    autotiling
    sway-contrib.grimshot
    swaysome
  ];

  wayland.windowManager.sway = with pkgs.rice; {
    # {{{
    enable = true;
    package = pkgs.sway;
    checkConfig = false; # swayfx fails to create a renderer when rebuilding
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
    # extraConfig = ''
    #   titlebar_separator disable
    #   titlebar_border_thickness 2
    # '';
  }; # }}}
}
