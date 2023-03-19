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
    displayManager.sx = {
      enable = true;
    };
    libinput.enable = true;
    layout = "pl";
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "tuigreet -tr --cmd sx";
      };
    };
  };

  fonts.fonts = with pkgs; [
    (nerdfonts.override {fonts = ["JetBrainsMono"];})
    roboto
    material-design-icons
  ];
}
