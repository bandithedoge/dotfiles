{
  config,
  pkgs,
  ...
}: let
  rice = import ../rice.nix {inherit pkgs;};
in {
  environment.systemPackages = with pkgs; [
    xorg.setxkbmap
    betterlockscreen
    arandr
  ];

  services.xserver = {
    enable = true;
    displayManager = {
      lightdm = {
        enable = true;
        background = rice.wallpaperBlurred;
        extraSeatDefaults = ''
          greeter-show-manual-login=false
          font-name="${rice.uiFont}"
        '';
        greeters.gtk = {
          enable = true;
          clock-format = "%A %d %B %T";
          indicators = ["~host" "~spacer" "~spacer" "~clock" "~power"];
          inherit (rice.gtk) theme iconTheme cursorTheme;
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
    libinput.mouse.accelProfile = "flat";
  };

  programs.xss-lock = {
    enable = true;
    lockerCommand = with rice; ''
      ${pkgs.betterlockscreen}/bin/betterlockscreen -l
    '';
  };

  services.accounts-daemon.enable = true;

  fonts.fonts = with pkgs; [
    (nerdfonts.override {fonts = ["JetBrainsMono"];})
    roboto
  ];

  security.polkit.enable = true;
}
