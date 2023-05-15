{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    winetricks
    xorg.setxkbmap
  ];

  services.xserver = {
    enable = true;
    displayManager = {
      lightdm = {
        enable = true;
        background = pkgs.rice.wallpaperBlurred;
        greeters.slick = {
          enable = true;
          inherit (pkgs.rice.gtk) theme iconTheme cursorTheme;
          font.name = pkgs.rice.uiFont;
        };
        greeters.gtk = {
          enable = false;
          inherit (pkgs.rice.gtk) theme iconTheme cursorTheme;
          clock-format = "%A %d %B %T";
          indicators = ["~host" "~spacer" "~clock" "~spacer" "~power"];
          extraConfig = with pkgs.rice; ''
            background = ${wallpaperBlurred}
            font-name = ${uiFont}
          '';
        };
      };
      sx.enable = true;
    };
    windowManager.session = [
      {
        name = "sx";
        start = ''
          ${pkgs.sx}/bin/sx
        '';
      }
    ];
    layout = "pl";
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

  security.polkit.enable = true;

  programs.steam.enable = true;

  services.flatpak.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
    ];
  };

  boot.kernel.sysctl = {
    "vm.mmap_min_addr" = 0;
  };
}
