{ config, pkgs, ... }:

let
  rice = import ../rice.nix;
in
{
  imports = [ /etc/nixos/hardware-configuration.nix ];

  environment.systemPackages = with pkgs; [ alsa-utils connman-gtk ];
  environment.shells = [ pkgs.fish pkgs.bashInteractive ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader = { systemd-boot.enable = true; };
    supportedFilesystems = [ "ntfs" ];
  };

  security = {
    rtkit.enable = true;
    pam.services.swaylock.text = ''
      auth include login
    '';
  };

  services.dbus = {
    enable = true;
    packages = with pkgs; [ dconf ];
  };

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
    roboto
  ];

  programs.dconf.enable = true;

  # drives {{{
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

  services.devmon.enable = true;
  programs.udevil.enable = true;
  # }}}

  # hardware {{{
  powerManagement = {
    enable = true;
    powertop.enable = true;
  };

  services.thermald.enable = true;
  services.tlp.enable = true;
  services.logind.lidSwitch = "hybrid-sleep";
  hardware.opengl.enable = true;

  services.kmonad = {
    enable = true;
    configfiles = [ ./kmonad.kbd ];
  };
  # }}}

  # networking {{{
  networking = { hostName = "thonkpad"; };

  services.connman = {
    enable = true;
    enableVPN = false;
  };
  services.openssh = {
    enable = true;
    startWhenNeeded = true;
  };

  networking.wireless = {
    enable = true;
    userControlled.enable = true;
  };
  # }}}

  # audio {{{
  services.pipewire = {
    enable = true;
    media-session.enable = false;
    wireplumber.enable = true;

    pulse.enable = true;
    jack.enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
  };

  musnix.enable = true;
  # }}}

  services.greetd = {
    enable = true;
    vt = 2;
    settings = {
      default_session = {
        command = "${
            pkgs.lib.makeBinPath [ pkgs.greetd.tuigreet ]
          }/tuigreet -tr --cmd ${rice.wm}";
      };
    };
  };

  time.timeZone = "Europe/Warsaw";

  users.users."bandithedoge" = {
    extraGroups = [ "wheel" "networkmanager" ];
    isNormalUser = true;
    uid = 1000;
  };

  programs.ccache.enable = true;
  programs.steam.enable = true;
}
