{pkgs, ...}: {
  system.stateVersion = "23.05";

  environment = {
    systemPackages = with pkgs; [
      alsa-utils
      ntfs3g
    ];
  };

  services.dbus = {
    enable = true;
    packages = with pkgs; [dconf];
  };

  programs.dconf.enable = true;

  services.devmon.enable = true;
  programs.udevil.enable = true;

  services.openssh.enable = true;

  programs.ccache.enable = true;
  programs.adb.enable = true;

  services.upower.enable = true;

  services.printing = {
    enable = true;
    cups-pdf.enable = true;
    drivers = with pkgs; [gutenprint];
  };

  security = {
    rtkit.enable = true;
    pam.services.gtklock.text = ''
      auth include login
    '';
    polkit.enable = true;
    sudo.enable = false;
    sudo-rs = {
      enable = true;
    };
  };

  users.mutableUsers = true;

  time.timeZone = "Europe/Warsaw";

  console = {
    colors = map (pkgs.lib.removePrefix "#") (with pkgs.rice; [
      base00
      base08
      base0B
      base09
      base0D
      base0E
      base0C
      base06
      base02
      base12
      base14
      base13
      base16
      base17
      base15
      base0F
    ]);
    useXkbConfig = true;
  };
}
