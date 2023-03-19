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

  hardware.cpu.intel.updateMicrocode = config.hardware.enableRedistributableFirmware;

  services.xserver.libinput.mouse.accelProfile = "flat";

  hardware.opengl.enable = true;
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
    BROWSER = "vivaldi";
  };

  system.stateVersion = "22.05";

  home-manager.users."bandithedoge" = {
    programs.alacritty.settings.font.size = 10.5;
  };
}
