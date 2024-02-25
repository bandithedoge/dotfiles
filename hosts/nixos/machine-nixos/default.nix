{pkgs, ...}: {
  imports = [./virt.nix];

  boot = {
    kernelPackages = pkgs.linuxPackages_cachyos;
    kernelParams = ["threadirqs"];
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  powerManagement.cpuFreqGovernor = "performance";

  services.irqbalance.enable = true;

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

  # services.replay-sorcery = {
  #   enable = true;
  #   enableSysAdminCapability = true;
  #   autoStart = true;
  #   settings = {
  #     videoInput = "hwaccel";
  #     videoFramerate = 60;
  #     controller = "command";
  #   };
  # };

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
    BROWSER = "firefox-nightly";
    WINEFSYNC = "1";
    ROC_ENABLE_PRE_VEGA = "1";
  };
}
