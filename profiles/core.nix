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
  nix = {
    settings = {
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://nixpkgs-wayland.cachix.org"
        "https://kira-bruneau.cachix.org"
        "https://bandithedoge.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
        "kira-bruneau.cachix.org-1:FJSccwNPRNHPBHN+qxAme2Svp537q7dDuHkqLnyOTaQ="
        "bandithedoge.cachix.org-1:ZtcHw1anyEa4t6H8m3o/ctYFrwYFPAwoENSvofamE6g="
      ];
      trusted-users = ["root" "@wheel" "bandithedoge"];
    };
    extraOptions = ''
      extra-experimental-features = nix-command flakes
      allow-dirty = true
      auto-optimise-store = true
      keep-derivations = false
      min-free = ${toString (512 * 1024 * 1024)}
      substitute = true
      warn-dirty = false
      pure-eval = false
      sandbox = relaxed
      log-lines = 50
    '';
  };

  environment.systemPackages = with pkgs; [
    alsa-utils
  ];

  security = {
    rtkit.enable = true;
    pam.services.swaylock.text = ''
      auth include login
    '';
    pki.certificateFiles = [/etc/ssl/certs/certyfikat.crt];
  };

  services.dbus = {
    enable = true;
    packages = with pkgs; [dconf];
  };

  programs.dconf.enable = true;

  services.devmon.enable = true;
  programs.udevil.enable = true;

  services.connman = {
    enable = true;
    enableVPN = false;
  };

  services.openssh = {
    enable = true;
  };

  programs.ssh = {
    startAgent = true;
  };

  time.timeZone = "Europe/Warsaw";

  programs.ccache.enable = true;
  programs.steam.enable = true;

  users.mutableUsers = true;
}
