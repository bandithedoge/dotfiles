{pkgs, ...}: {
  system.stateVersion = "23.11";

  hardware.enableRedistributableFirmware = true;

  environment = {
    systemPackages = with pkgs; [
      alsa-utils
      ntfs3g
    ];
  };
  services = {
    dbus = {
      enable = true;
      packages = with pkgs; [dconf];
    };

    printing = {
      enable = true;
      cups-pdf.enable = true;
      drivers = with pkgs; [gutenprint];
    };

    devmon.enable = true;
    openssh.enable = true;
    upower.enable = true;
  };

  programs = {
    adb.enable = true;
    ccache.enable = true;
    dconf.enable = true;
    nix-ld.enable = true;
    udevil.enable = true;
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
