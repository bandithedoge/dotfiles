{
  suites,
  pkgs,
  ...
}: {
  imports = with suites;
    base
    ++ os-specific
    ++ gaming;

  environment.systemPackages = with pkgs; [
    wine-tkg
  ];

  # displays {{{
  services.xserver = {
    xrandrHeads = [
      "DVI-D-0"
      {
        output = "HDMI-A-0";
        primary = true;
        monitorConfig = ''
          Option "Position" "1920 45"
        '';
      }
    ];
  };
  # }}}

  # hardware {{{
  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_xanmod_latest;
    kernelParams = ["threadirqs"];
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

  networking.useDHCP = true;

  programs.openvpn3.enable = true;

  hardware.cpu.intel.updateMicrocode = true;

  services.xserver.libinput.mouse.accelProfile = "flat";

  hardware.opengl.enable = true;

  services.printing = {
    enable = true;
    drivers = with pkgs; [gutenprint];
  };

  powerManagement.cpuFreqGovernor = "performance";
  # }}}

  # drives {{{
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/418f785a-3ee1-423b-bf2d-f3a3329c77d2";
      fsType = "btrfs";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/90CD-E31C";
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

  swapDevices = [{device = "/dev/disk/by-uuid/80f501f4-d78a-48e4-96d2-e723122fb0d5";}];
  # }}}

  environment.variables = {
    BROWSER = "firefox";
    WINEFSYNC = "1";
  };

  system.stateVersion = "22.05";
}
