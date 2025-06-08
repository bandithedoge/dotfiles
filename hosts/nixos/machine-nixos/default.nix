{
  pkgs,
  ...
}:
{
  imports = [ ./virt.nix ];

  boot = {
    kernelParams = [
      "preempt=full"
      "mitigations=off"
    ];
    kernelModules = [ "wireguard" ];
    blacklistedKernelModules = [ "iTCO_wdt" ];
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  networking = {
    hostName = "machine-nixos";
  };

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
      enable = false;
      extraPackages = with pkgs; [
        intel-media-driver
        intel-vaapi-driver
        rocmPackages.clr
        rocmPackages.clr.icd
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
      opencl.enable = true;
    };
  };

  programs.corectrl = {
    enable = true;
  };

  services = {
    pipewire = {
      extraConfig.pipewire = {
        "10-loopback-mono-mic"."context.modules" = [
          {
            name = "libpipewire-module-loopback";
            args = {
              "node.description" = "USB Audio CODEC [MONO]";
              "capture.props" = {
                "node.name" = "capture.mono-microphone";
                "audio.position" = [ "FL" ];
                "target.object" = "alsa_input.usb-Burr-Brown_from_TI_USB_Audio_CODEC-00.analog-stereo-input";
                "stream.dont-remix" = true;
                "node.passive" = true;
              };
              "playback.props" = {
                "media.class" = "Audio/Source";
                "node.name" = "mono-microphone";
                "audio.position" = [ "MONO" ];
                "priority.session" = 2500;
              };
            };
          }
        ];

        "10-autoeq"."context.modules" = [
          {
            name = "libpipewire-module-filter-chain";
            args = {
              "node.description" = "AutoEQ Sink";
              "media.name" = "AutoEQ Sink";
              "filter.graph" = {
                nodes = [
                  {
                    type = "builtin";
                    label = "convolver";
                    name = "conv";
                    config = {
                      filename = ./conv.wav;
                    };
                  }
                ];
              };
              "audio.channels" = 2;
              "audio.position" = [
                "FL"
                "FR"
              ];
              "capture.props" = {
                "node.name" = "effect_input.filter-chain-autoeq";
                "media.class" = "Audio/Sink";
                "priority.session" = 1500;
              };
              "playback.props" = {
                "node.name" = "effect_output.filter-chain-autoeq";
                "node.target" = "alsa_output.usb-Burr-Brown_from_TI_USB_Audio_CODEC-00.analog-stereo-output";
                "node.passive" = true;
              };
            };
          }
        ];
      };
      wireplumber.extraConfig = {
        "10-pro-audio"."monitor.alsa.rules" = [
          {
            matches = [ { "alsa.id" = "USB Audio"; } ];
            actions.update-props."device.profile" = "pro-audio";
          }
        ];

        "51-disable-builtin-audio"."monitor.alsa.rules" = [
          {
            matches = [
              { "alsa.id" = "~HDMI"; }
              { "alsa.id" = "~ALC887-VD"; }
            ];
            actions.update-props."device.disabled" = true;
          }
        ];
      };
    };

    scx = {
      enable = true;
      package = pkgs.scx_git.rustscheds;
      scheduler = "scx_flash";
    };

    btrfs.autoScrub.enable = true;
  };

  nix.settings.max-jobs = 6;

  musnix = {
    enable = true;
    rtcqs.enable = true;
    rtirq.enable = true;
  };
}
