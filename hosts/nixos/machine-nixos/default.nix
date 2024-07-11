{pkgs, ...}: {
  imports = [./virt.nix];

  boot = {
    kernelPackages = pkgs.linuxPackages_cachyos-lto;
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

    wireguard.enable = true;
  };

  programs.openvpn3.enable = true;

  services.ananicy = {
    enable = true;
    package = pkgs.ananicy-cpp;
    rulesProvider = pkgs.ananicy-cpp-rules;
  };

  # TODO
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
      options = [
        "compress=zstd"
      ];
    };
    "/boot" = {
      device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };
    "/mnt/data" = {
      device = "/dev/disk/by-label/data";
      fsType = "btrfs";
      options = [
        "compress=zstd"
      ];
    };
    "/mnt/soft" = {
      device = "/dev/disk/by-label/soft";
      fsType = "btrfs";
      options = [
        "compress=zstd"
      ];
    };
    "/mnt/buttplug" = {
      device = "/dev/disk/by-label/buttplug";
      fsType = "btrfs";
      options = [
        "compress=zstd"
      ];
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
    # ROC_ENABLE_PRE_VEGA = "1";
  };

  # chaotic = {
  #   mesa-git = {
  #     enable = true;
  #     extraPackages = with pkgs; [
  #       rocmPackages.clr.icd
  #     ];
  #   };
  # };

  hardware.amdgpu = {
    initrd.enable = true;
    opencl.enable = true;
  };
}
