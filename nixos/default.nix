{ config, pkgs, ... }:

{
  imports = [ /etc/nixos/hardware-configuration.nix ];

  environment.systemPackages = with pkgs; [ connman-gtk alsa-utils ];

  boot = {
    loader = { systemd-boot.enable = true; };
    supportedFilesystems = [ "ntfs" ];
  };

  fileSystems = {
    "/mnt/data" = {
      device = "/dev/disk/by-label/shit";
      fsType = "ntfs";
      options = [
        "rw"
        "uid=${builtins.toString config.users.users."bandithedoge".uid}"
      ];
    };
  };

  powerManagement = {
    enable = true;
    powertop.enable = true;
  };

  services.thermald.enable = true;
  services.tlp.enable = true;

  services.logind = { lidSwitch = "hybrid-sleep"; };

  networking = { hostName = "thonkpad"; };

  services.connman = { enable = true; };

  networking.wireless = {
    enable = true;
    userControlled.enable = true;
  };

  hardware.opengl.enable = true;

  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    media-session.enable = true;

    pulse.enable = true;
    jack.enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${
            pkgs.lib.makeBinPath [ pkgs.greetd.tuigreet ]
          }/tuigreet -tr --cmd sway";
      };
    };
  };

  time.timeZone = "Europe/Warsaw";

  users.users."bandithedoge" = {
    extraGroups = [ "wheel" "networkmanager" ];
    isNormalUser = true;
    uid = 1001;
  };
}
