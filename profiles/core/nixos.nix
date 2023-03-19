{pkgs, ...}: {
  imports = [../../nix.nix];

  system.stateVersion = "22.11";

  environment.systemPackages = with pkgs; [
    alsa-utils
    ntfs3g
  ];

  security = {
    rtkit.enable = true;
    pam.services.swaylock.text = ''
      auth include login
    '';
    polkit.enable = true;
    sudo.extraConfig = ''
      Defaults insults
    '';
  };

  services.dbus = {
    enable = true;
    packages = with pkgs; [dconf];
  };

  programs.dconf.enable = true;

  services.devmon.enable = true;
  programs.udevil.enable = true;

  services.openssh = {
    enable = true;
  };

  programs.ssh = {
    startAgent = true;
  };

  time.timeZone = "Europe/Warsaw";

  programs.ccache.enable = true;

  users.mutableUsers = true;

  programs.adb.enable = true;

  services.upower.enable = true;
}
