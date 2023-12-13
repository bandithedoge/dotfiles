{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    greetd.greetd
    rice.gtk.cursorTheme.package
    rice.gtk.iconTheme.package
    rice.gtk.theme.package
    winetricks
    xorg.setxkbmap
  ];

  services.xserver.displayManager.sessionPackages = with pkgs; [sway];

  services.greetd.enable = true;

  programs.regreet = {
    enable = true;
    cageArgs = ["-s" "-m" "last"];
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

  programs.hyprland.enable = true;

  services.accounts-daemon.enable = true;

  programs.ns-usbloader.enable = true;

  services.flatpak.enable = true;

  programs.system-config-printer.enable = true;

  xdg.portal = {
    enable = true;
    config.hyprland.default = ["wlr" "gtk"];
  };

  gtk.iconCache.enable = true;

  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
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

  boot.kernel.sysctl = {
    "vm.mmap_min_addr" = 0;
  };
}
