{ config, pkgs, ... }:

{
  imports = [ /etc/nixos/hardware-configuration.nix ];

  environment.systemPackages = with pkgs; [ connman-gtk ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
      grub = {
        enable = true;
        efiSupport = true;
        devices = [ "nodev" ];
        extraEntries = ''
          menuentry "Windows 10" {
            insmod part_gpt
            insmod fat
            insmod search_fs_uuid
            insmod chain
            search --fs-uuid --set=root 7059-2F1B
            chainloader /EFI/Microsoft/Boot/bootmgfw.efi
          }
        '';
      };
    };
    supportedFilesystems = [ "ntfs" ];
  };

  fileSystems = {
    "/mnt/data" = {
      device = "/dev/sda3";
      fsType = "ntfs";
      options = [ "rw" ];
    };
    "/mnt/windows" = {
      device = "/dev/sdb3";
      fsType = "ntfs";
      options = [ "rw" ];
    };
  };

  networking = { hostName = "thonkpad"; };

  services.connman = {
    enable = true;
    wifi.backend = "iwd";
  };

  hardware.opengl.enable = true;

  fonts.fonts = with pkgs; [ (nerdfonts.override { fonts = [ "FiraCode" ]; }) ];

  /* services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${
            pkgs.lib.makeBinPath [ pkgs.greetd.tuigreet ]
          }/tuigreet -tr --cmd sway";
      };
    };
  }; */

  time.timeZone = "Europe/Warsaw";
}
