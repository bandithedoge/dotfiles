{pkgs, ...}: {
  imports = [./virt.nix];

  boot = {
    kernelPackages = pkgs.linuxPackages_cachyos;
    kernelParams = ["threadirqs" "preempt=full"];
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  powerManagement.cpuFreqGovernor = "performance";

  networking = {
    hostName = "machine-nixos";

    networkmanager = {
      enable = true;
      plugins = with pkgs; [networkmanager-openvpn];
    };

    wireguard.enable = true;
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

  chaotic = {
    mesa-git = {
      enable = true;
      extraPackages = with pkgs; [
        intel-media-driver
        intel-vaapi-driver
        # https://nixpk.gs/pr-tracker.html?pr=370180
        # rocmPackages.clr
        # rocmPackages.clr.icd
      ];
      extraPackages32 = with pkgs; [
        driversi686Linux.intel-media-driver
        driversi686Linux.intel-vaapi-driver
      ];
    };
  };

  hardware = {
    amdgpu = {
      initrd.enable = true;
      # https://nixpk.gs/pr-tracker.html?pr=370180
      # opencl.enable = true;
    };
  };

  programs.gpu-screen-recorder.enable = true;

  services.pipewire = {
    extraConfig.pipewire = {
      "10-loopback-mono-mic" = {
        "context.modules" = [
          {
            name = "libpipewire-module-loopback";
            args = {
              "node.description" = "USB Audio CODEC [MONO]";
              "capture.props" = {
                "node.name" = "capture.mono-microphone";
                "audio.position" = ["FL"];
                "target.object" = "alsa_input.usb-Burr-Brown_from_TI_USB_Audio_CODEC-00.analog-stereo-input";
                "stream.dont-remix" = true;
                "node.passive" = true;
              };
              "playback.props" = {
                "media.class" = "Audio/Source";
                "node.name" = "mono-microphone";
                "audio.position" = ["MONO"];
              };
            };
          }
        ];
      };
    };
    wireplumber.extraConfig = {
      "51-disable-builtin-audio" = {
        "monitor.alsa.rules" = [
          {
            matches = [{"alsa.id" = "~HDMI|PCH";}];
            actions.update-props = {
              "device.disabled" = true;
            };
          }
        ];
      };
    };
  };
}
