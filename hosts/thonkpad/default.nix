{
  suites,
  config,
  pkgs,
  ...
}: {
  imports = with suites;
    base
    ++ gui
    ++ audio;

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader = {systemd-boot.enable = true;};
    supportedFilesystems = ["ntfs"];
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
  services.logind.lidSwitch = "hybrid-sleep";
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.trackpoint.enable = true;

  services.kmonad = {
    enable = true;
    configfiles = [./kmonad.kbd];
  };

  networking.wireless = {
    enable = true;
    userControlled.enable = true;
  };

  boot.initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "rtsx_pci_sdmmc"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/11ac846a-2311-4cc5-871c-64e52c45a009";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/C106-5693";
    fsType = "vfat";
  };

  swapDevices = [{device = "/dev/disk/by-uuid/7eaade78-9c5e-4c25-8de4-20cf7ced3e72";}];
}
