{
  pkgs,
  config,
  ...
}: {
  environment.systemPackages = with pkgs;
    [
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
    ns-usbloader.enable = true;
    system-config-printer.enable = true;
  };

  services = {
    xserver.displayManager.sessionPackages = with pkgs; [swayfx];
    greetd.enable = true;
    accounts-daemon.enable = true;
    flatpak.enable = true;
  };

  xdg.portal = {
    enable = true;
    config.sway.default = ["wlr" "gtk"];
    extraPortals = with pkgs; [
      xdg-desktop-portal-wlr
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
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [pkgs.rice.monoFont];
        sansSerif = [pkgs.rice.uiFont];
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
    kernel.sysctl = {
      "vm.mmap_min_addr" = 0;
    };
  };

  security.pam = {
    services.swaylock = {};
    loginLimits = [
      {
        domain = "bandithedoge";
        type = "hard";
        item = "nofile";
        value = "524288";
      }
    ];
  };
}
