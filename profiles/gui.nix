{
  config,
  pkgs,
  ...
}: let
  rice = import ../rice.nix;
  launchwm = pkgs.writeShellScriptBin "launchwm" ''
    #!/usr/bin/env bash
    XKB_DEFAULT_LAYOUT=pl ${rice.wm}
  '';
in {
  environment.systemPackages = with pkgs; [
    connman-gtk
    greetd.tuigreet
    launchwm
    xorg.setxkbmap
  ];

  services.xserver = {
    enable = true;
    displayManager = {
      lightdm = {
        enable = true;
        greeters.gtk = {
          enable = true;
          clock-format = "%A %d %B %T";
          indicators = ["~host" "~spacer" "~spacer" "~clock" "~power"];
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
    libinput.enable = true;
    layout = "pl";
  };

  programs.xss-lock = {
    enable = true;
    lockerCommand = "${pkgs.i3lock-fancy-rapid}/bin/i3lock-fancy-rapid";
  };

  fonts.fonts = with pkgs; [
    (nerdfonts.override {fonts = ["JetBrainsMono"];})
    roboto
    material-design-icons
  ];
}
