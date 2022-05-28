{pkgs, ...}: {
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
