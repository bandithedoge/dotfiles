{pkgs, ...}: {
  environment.systemPackages = with pkgs; [];

  # hardware {{{
  boot = {
    kernelPackages = pkgs.linuxPackages_cachyos;
    initrd = {
      availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod"];
      kernelModules = [];
    };
    kernelModules = ["kvm-intel"];
    extraModulePackages = [];
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  powerManagement.cpuFreqGovernor = "performance";
  # }}}

  networking = {
    hostName = "machine-nixos";

    networkmanager = {
      enable = true;
      plugins = with pkgs; [networkmanager-openvpn];
    };

    wireguard = {
      enable = true;
    };
  };

  programs.openvpn3.enable = true;

  services.ananicy = {
    enable = true;
    package = pkgs.ananicy-cpp;
    rulesProvider = pkgs.ananicy-cpp-rules;
  };

  # drives {{{
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/nixos";
      fsType = "btrfs";
    };
    "/boot" = {
      device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };
    "/mnt/data" = {
      device = "/dev/disk/by-label/data";
      fsType = "btrfs";
    };
    "/mnt/soft" = {
      device = "/dev/disk/by-label/soft";
      fsType = "btrfs";
    };
    "/mnt/buttplug" = {
      device = "/dev/disk/by-label/buttplug";
      fsType = "btrfs";
    };
  };

  swapDevices = [
    {
      device = "/swap/swapfile";
      size = 4 * 1024;
    }
  ];
  # }}}

  environment.variables = {
    BROWSER = "firefox";
    WINEFSYNC = "1";
  };
}
