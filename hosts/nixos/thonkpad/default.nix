{
  suites,
  pkgs,
  ...
}: {
  imports = with suites;
    base
    ++ gui
    ++ audio
    ++ gaming
    ++ virt;

  environment = {
    systemPackages = with pkgs; [
      connman-gtk
      wineWowPackages.staging
    ];
    variables = {
      BROWSER = "qutebrowser";
    };
  };

  # hardware {{{
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = ["kvm-intel"];
    extraModulePackages = [];
    initrd = {
      availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "rtsx_pci_sdmmc"];
      kernelModules = [];
    };
    loader.systemd-boot.enable = true;
    supportedFilesystems = ["ntfs"];
  };

  powerManagement = {
    enable = true;
    powerDownCommands = ''
      awesome-client "vicious.suspend()"
    '';
    resumeCommands = ''
      awesome-client "vicious.activate()"

      modprobe -r psmouse
      modprobe psmouse
    '';
    powertop.enable = true;
  };

  services.acpid.enable = true;
  # services.thinkfan.enable = true;
  services.thermald.enable = true;
  services.tlp.enable = true;
  services.logind.lidSwitch = "suspend";

  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
    trackpoint.enable = true;
  };

  services.xserver.libinput = {
    enable = true;
    touchpad.tapping = false;
  };
  # }}}

  # drives {{{
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/11ac846a-2311-4cc5-871c-64e52c45a009";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/C106-5693";
      fsType = "vfat";
    };
  };

  swapDevices = [{device = "/dev/disk/by-uuid/7eaade78-9c5e-4c25-8de4-20cf7ced3e72";}];
  # }}}

  # keyboard {{{
  services.kmonad = {
    enable = true;
    package = pkgs.bandithedoge.haskellPackages.kmonad;
    keyboards.internal = {
      name = "laptop-internal";
      device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
      defcfg = {
        enable = true;
        fallthrough = true;
      };
      config = builtins.readFile ./kmonad.kbd;
    };
  };
  # }}}

  # networking {{{
  networking.wireless = {
    enable = true;
    userControlled.enable = true;
  };

  services.connman = {
    enable = true;
    enableVPN = false;
  };

  # what the fuck is this
  security.pki.certificateFiles = [/etc/ssl/certs/certyfikat.crt];
  # }}}
}
