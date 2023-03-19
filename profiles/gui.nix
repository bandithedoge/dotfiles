{
  config,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    arandr
    betterlockscreen
    winetricks
    xorg.setxkbmap
  ];

  services.xserver = {
    enable = true;
    displayManager = {
      lightdm = {
        enable = true;
        background = pkgs.rice.wallpaperBlurred;
        extraSeatDefaults = ''
          greeter-show-manual-login=false
          font-name="${pkgs.rice.uiFont}"
        '';
        greeters.gtk = {
          enable = true;
          clock-format = "%A %d %B %T";
          indicators = ["~host" "~spacer" "~spacer" "~clock" "~power"];
          inherit (pkgs.rice.gtk) theme iconTheme cursorTheme;
        };
      };
      sx = {
        enable = true;
      };
      defaultSession = "none+sx";
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

  programs.xss-lock = {
    enable = true;
    lockerCommand = ''
      ${pkgs.betterlockscreen}/bin/betterlockscreen -l
    '';
  };

  services.accounts-daemon.enable = true;

  fonts.fonts = with pkgs; [
    (nerdfonts.override {fonts = ["JetBrainsMono"];})
    roboto
  ];

  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      monospace = [pkgs.rice.monoFont];
      sansSerif = [pkgs.rice.uiFont];
    };
  };

  security.polkit.enable = true;

  programs.steam.enable = true;

  services.flatpak.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [xdg-desktop-portal-gtk];
  };
}
