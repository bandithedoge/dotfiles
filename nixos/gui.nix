{
  pkgs,
  config,
  ...
}: {
  environment.systemPackages = with pkgs; [
    greetd.greetd
    qt5.qtwayland
    qt6.qtwayland
    rice.gtk.cursorTheme.package
    rice.gtk.iconTheme.package
    rice.gtk.theme.package
    winetricks
    xorg.setxkbmap
  ];

  programs = {
    regreet = {
      enable = true;
      cageArgs = ["-s" "-d" "-m" "last"];
      settings = with pkgs.rice; {
        background = {
          path = wallpaperBlurred;
          fit = "Fill";
        };
        GTK = {
          application_prefer_dark_theme = true;
          cursor_theme_name = gtk.cursorTheme.name;
          font = "${uiFont} 16";
          icon_theme_name = gtk.iconTheme.name;
          theme_name = gtk.theme.name;
        };
      };
    };

    sway = {
      enable = true;
      package = null;
      extraPackages = [];
    };

    river = {
      enable = true;
      package = null;
      extraPackages = [];
    };

    gnome-disks.enable = true;
    hyprland.enable = false;
    ns-usbloader.enable = true;
    system-config-printer.enable = true;
  };

  services = {
    xserver.displayManager.sessionPackages = with pkgs; [
      swayfx
      river
    ];

    accounts-daemon.enable = true;
    flatpak.enable = true;
    gnome.gnome-keyring.enable = true;
    greetd.enable = true;
  };

  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
    ];
  };

  gtk.iconCache.enable = true;

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      (nerdfonts.override {fonts = ["JetBrainsMono"];})
      bandithedoge.symbols-nerd-font
      emojione
      roboto
      roboto-slab
      twemoji-color-font
    ];
    fontconfig = {
      enable = true;
      hinting.autohint = true;
      defaultFonts = with pkgs.rice; {
        monospace = [monoFont];
        sansSerif = [uiFont];
        serif = [serifFont];
        emoji = [emojiFont];
      };
    };
  };

  qt = {
    enable = true;
    style = "adwaita-dark";
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
    services.waylock = {};
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

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    setLdLibraryPath = true;
  };
}
