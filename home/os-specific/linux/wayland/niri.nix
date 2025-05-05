{
  pkgs,
  config,
  ...
}: {
  home.packages = with pkgs; [
    sway-contrib.grimshot
  ];

  programs.niri = {
    enable = true;
    package = pkgs.niri;
    settings = {
      hotkey-overlay.skip-at-startup = true;
      prefer-no-csd = true;

      spawn-at-startup = [
        {command = ["swaybg" "-i" (builtins.toString pkgs.rice.wallpaper) "-m" "fill"];}
        {command = ["waybar"];}
      ];

      environment = {
        DISPLAY = ":0";
      };

      input = {
        focus-follows-mouse.enable = true;
        keyboard = {
          repeat-delay = 300;
          xkb.layout = "pl";
        };
        mouse.accel-profile = "flat";
        touchpad = {
          natural-scroll = true;
          scroll-method = "two-finger";
          tap = false;
        };
      };

      outputs = {
        "DP-2".position = {
          x = 0;
          y = 0;
        };
        "DP-3".position = {
          x = 1920;
          y = 0;
        };
      };

      layout = with pkgs.rice; {
        focus-ring = {
          width = 2;
          active.color = base0F;
          inactive.color = base00;
        };
        insert-hint.display.color = base0F + "80";
        always-center-single-column = true;
        default-column-width = {};
        gaps = 10;
        preset-column-widths = [
          {proportion = 0.3;}
          {proportion = 0.5;}
          {proportion = 0.7;}
          {proportion = 1.0;}
        ];
        struts = {
          left = 15;
          right = 15;
        };
        tab-indicator = {
          position = "top";
          place-within-column = true;
        };
        shadow = {
          enable = true;
          offset = {
            x = 0;
            y = 0;
          };
        };
      };

      window-rules = [
        (with pkgs.rice; {
          matches = [{is-window-cast-target = true;}];
          focus-ring = {
            active.color = base0E;
            inactive.color = base03;
          };
          border.inactive.color = base03;
          tab-indicator = {
            active.color = base0E;
            inactive.color = base03;
          };
        })
        {
          matches = [
            {
              app-id = "^com\\.mitchellh\\.ghostty$";
              is-focused = false;
            }
          ];
          opacity = 0.85;
          draw-border-with-background = false;
        }
        {
          matches = [
            {
              app-id = "^com\\.mitchellh\\.ghostty$";
              is-focused = true;
            }
          ];
          opacity = 0.95;
          draw-border-with-background = false;
        }
        {
          matches = [
            {app-id = "librewolf";}
            {app-id = "floorp";}
            {app-id = "net.lutris.Lutris";}
          ];
          clip-to-geometry = true;
        }
        {
          matches = [
            {app-id = "^Ardour-\\d+\\.\\d+\\.\\d+$";}
            {
              app-id = "^$";
              title = "^$";
            }
          ];
          open-floating = true;
        }
      ];

      layer-rules = [
        {
          matches = [
            {namespace = "waybar";}
            {namespace = "rofi";}
          ];
          shadow.enable = true;
        }
        {
          matches = [{namespace = "^notifications$";}];
          block-out-from = "screencast";
        }
      ];

      switch-events = {
        lid-close.action.spawn = ["loginctl" "lock-session"];
      };

      binds = with config.lib.niri.actions; {
        "Mod+Shift+Slash".action = show-hotkey-overlay;
        "Mod+Return".action.spawn = pkgs.rice.terminal;
        "Mod+Space".action.spawn = pkgs.lib.splitString " " pkgs.rice.menu;
        "Mod+Backspace".action.spawn = "wlr-which-key";
        "Mod+Ctrl+p".action.spawn = "keepmenu";
        "Mod+Ctrl+q".action.spawn = ["loginctl" "terminate-user" ""];

        Print.action = screenshot;
        "Shift+Print".action.spawn =
          builtins.toString (pkgs.writeShellScript "save-replay"
            "killall -SIGUSR1 gpu-screen-recorder && notify-send -u low 'GPU Screen Recorder' 'Replay saved' -i com.dec05eba.gpu_screen_recorder -a 'GPU Screen Recorder'");

        XF86AudioMute = {
          action.spawn = ["wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"];
          allow-when-locked = true;
        };
        XF86AudioRaiseVolume = {
          action.spawn = ["wpctl" "set-volume" "-l" "1.5" "@DEFAULT_AUDIO_SINK@" "5%+"];
          allow-when-locked = true;
        };
        XF86AudioLowerVolume = {
          action.spawn = ["wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%-"];
          allow-when-locked = true;
        };
        XF86AudioPlay = {
          action.spawn = ["mpc" "toggle"];
          allow-when-locked = true;
        };
        XF86AudioPrev = {
          action.spawn = ["mpc" "prev"];
          allow-when-locked = true;
        };
        XF86AudioNext = {
          action.spawn = ["mpc" "next"];
          allow-when-locked = true;
        };

        "Mod+h".action = focus-monitor-previous;
        "Mod+j".action = focus-window-down-or-column-right;
        "Mod+k".action = focus-window-up-or-column-left;
        "Mod+l".action = focus-monitor-next;
        "Mod+Shift+h".action = move-window-to-monitor-previous;
        "Mod+Shift+j".action = swap-window-right;
        "Mod+Shift+k".action = swap-window-left;
        "Mod+Shift+l".action = move-window-to-monitor-next;
        "Mod+Shift+t".action = switch-focus-between-floating-and-tiling;

        "Mod+w".action = close-window;
        "Mod+t".action = toggle-window-floating;
        "Mod+f".action = fullscreen-window;
        "Mod+r".action = switch-preset-column-width;
        "Mod+Shift+r".action = expand-column-to-available-width;
        "Mod+Comma".action = consume-window-into-column;
        "Mod+Period".action = expel-window-from-column;
        "Mod+g".action = toggle-column-tabbed-display;

        "Mod+WheelScrollDown".action = focus-workspace-down;
        "Mod+WheelScrollUp".action = focus-workspace-up;

        "Mod+1".action = focus-workspace 1;
        "Mod+2".action = focus-workspace 2;
        "Mod+3".action = focus-workspace 3;
        "Mod+4".action = focus-workspace 4;
        "Mod+5".action = focus-workspace 5;
        "Mod+6".action = focus-workspace 6;
        "Mod+7".action = focus-workspace 7;
        "Mod+8".action = focus-workspace 8;
        "Mod+9".action = focus-workspace 9;
        "Mod+Shift+1".action = move-column-to-workspace 1;
        "Mod+Shift+2".action = move-column-to-workspace 2;
        "Mod+Shift+3".action = move-column-to-workspace 3;
        "Mod+Shift+4".action = move-column-to-workspace 4;
        "Mod+Shift+5".action = move-column-to-workspace 5;
        "Mod+Shift+6".action = move-column-to-workspace 6;
        "Mod+Shift+7".action = move-column-to-workspace 7;
        "Mod+Shift+8".action = move-column-to-workspace 8;
        "Mod+Shift+9".action = move-column-to-workspace 9;
      };
    };
  };

  xdg.portal = {
    extraPortals = with pkgs; [xdg-desktop-portal-gtk];
    config.niri = {
      default = ["gnome" "gtk"];
      "org.freedesktop.impl.portal.FileChooser" = "gtk";
      "org.freedesktop.impl.portal.Notification" = "gtk";
      "org.freedesktop.impl.portal.Secret" = "gnome-keyring";
    };
  };

  services.swayidle = {
    events = [
      {
        event = "before-sleep";
        command = "niri msg action power-off-monitors";
      }
      {
        event = "after-resume";
        command = "niri msg action power-on-monitors";
      }
    ];
    timeouts = [
      {
        timeout = 330;
        command =
          if (config.hostname == "thonkpad")
          then "systemctl suspend"
          else "niri msg action power-off-monitors";
      }
    ];
  };

  systemd.user.services = {
    xwayland-satellite = {
      Unit = {
        Description = "Xwayland outside your Wayland";
        BindsTo = ["graphical-session.target"];
        PartOf = ["graphical-session.target"];
        After = ["graphical-session.target"];
        Requisite = ["graphical-session.target"];
      };
      Service = {
        Type = "notify";
        NotifyAccess = "all";
        ExecStart = "${pkgs.lib.getExe pkgs.xwayland-satellite} :0";
        StandardOutput = "journal";
        Restart = "on-failure";
      };
      Install.WantedBy = ["graphical-session.target"];
    };
  };
}
