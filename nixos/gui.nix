{
  pkgs,
  config,
  ...
}: {
  environment = {
    systemPackages = with pkgs; [
      greetd.greetd
      qt5.qtwayland
      qt6.qtwayland
      rice.gtk.cursorTheme.package
      rice.gtk.iconTheme.package
      rice.gtk.theme.package
      xorg.setxkbmap
    ];
  };

  programs = {
    regreet = {
      enable = true;
      cageArgs = ["-s" "-d" "-m" "last"];
      inherit (pkgs.rice.gtk) theme;
      cursorTheme = {
        inherit (pkgs.rice.gtk.cursorTheme) name package;
      };
      settings.background = with pkgs.rice; {
        path = wallpaperBlurred;
        fit = "Fill";
      };
      extraCss = builtins.readFile (pkgs.rice.compileSCSS ../gtk.scss);
    };

    hyprland = {
      enable = true;
      withUWSM = true;
    };

    gnome-disks.enable = true;
    ns-usbloader.enable = true;
    system-config-printer.enable = true;
  };

  services = {
    # displayManager.sessionPackages = let
    #   waylandSession = let
    #     sessionItem = pkgs.makeDesktopItem {
    #       name = "wayland";
    #       desktopName = "Wayland";
    #       comment = "Wayland session";
    #       exec = pkgs.writeShellScript "wayland-session.sh" ''
    #         source /etc/profiles/per-user/$USER/etc/profile.d/hm-session-vars.sh
    #         exec systemd-cat --identifier=${pkgs.rice.wm} ${pkgs.rice.wm} "$@"
    #       '';
    #     };
    #   in
    #     (pkgs.writeTextFile {
    #       name = "wayland.desktop";
    #       text = builtins.readFile "${sessionItem}/share/applications/wayland.desktop";
    #       destination = "/share/wayland-sessions/wayland.desktop";
    #     })
    #     .overrideAttrs (_: {passthru.providedSessions = ["wayland"];});
    # in [
    #   waylandSession
    # ];

    accounts-daemon.enable = true;
    gnome.gnome-keyring.enable = true;
    greetd.enable = true;
    flatpak.enable = true;
  };

  xdg = {
    portal = {
      enable = true;
      wlr.enable = true;
      config.sway.default = ["wlr" "gtk"];
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
      ];
    };

    terminal-exec.enable = true;
  };

  gtk.iconCache.enable = true;

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      nerd-fonts.symbols-only
      rice.monoFontPackage
      rice.uiFontPackage
      roboto-slab
      twemoji-color-font
    ];
    fontconfig = {
      enable = true;
      # hinting.autohint = true;
      defaultFonts = with pkgs.rice; {
        monospace = [monoFont];
        sansSerif = [uiFont];
        serif = [serifFont];
      };
    };
  };

  boot = {
    extraModulePackages = with config.boot.kernelPackages; [v4l2loopback];
    kernelModules = ["v4l2loopback"];
    extraModprobeConfig = ''
      options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
    '';
    kernel.sysctl = {
      "vm.mmap_min_addr" = 0;
    };
  };

  security.pam = {
    services = {
      waylock = {};
      gtklock = {};
    };
    loginLimits = [
      {
        domain = "*";
        type = "hard";
        item = "nofile";
        value = "2097152";
      }
      {
        domain = "@realtime";
        type = "-";
        item = "rtprio";
        value = "99";
      }
      {
        domain = "@realtime";
        type = "-";
        item = "memlock";
        value = "unlimited";
      }
    ];
  };

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
}
