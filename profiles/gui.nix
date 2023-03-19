{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    winetricks
    xorg.setxkbmap
    rice.gtk.theme.package
    rice.gtk.iconTheme.package
  ];

  services.greetd = {
    enable = true;
    settings.default_session = {
      command = "${pkgs.cage}/bin/cage -s -- ${pkgs.greetd.regreet}/bin/regreet";
      user = "greeter";
    };
  };

  systemd.tmpfiles.rules = [
    "d /var/log/regreet 0755 greeter greeter - -"
    "d /var/cache/regreet 0755 greeter greeter - -"
  ];

  environment.etc."greetd/regreet.toml".source = with pkgs.rice;
    (pkgs.formats.toml {}).generate "regreet.toml" {
      background = wallpaperBlurred;
      GTK = {
        application_prefer_dark_theme = true;
        cursor_theme_name = gtk.cursorTheme.name;
        font_name = "${uiFont} 16";
        icon_theme_name = gtk.iconTheme.name;
        theme_name = gtk.theme.name;
      };
    };

  services.xserver = {
    enable = false;
    displayManager = {
      sessionPackages = with pkgs; [river];
    };
  };

  services.accounts-daemon.enable = true;

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      (nerdfonts.override {fonts = ["JetBrainsMono"];})
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

  security = {
    polkit.enable = true;
    pam.services.waylock = {};
  };

  programs.steam.enable = true;

  services.flatpak.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-wlr
    ];
  };
}
