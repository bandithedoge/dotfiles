{pkgs, ...}: {
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
    kernelPackages = pkgs.linuxPackages_cachyos;
    kernelModules = ["kvm-intel"];
    initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "rtsx_pci_sdmmc"];
    loader.systemd-boot.enable = true;
    supportedFilesystems = ["ntfs"];
  };

  powerManagement = {
    enable = true;
    cpuFreqGovernor = pkgs.lib.mkForce "conservative";
    powerUpCommands = ''
      ${pkgs.kmod}/bin/modprobe -r psmouse
      ${pkgs.kmod}/bin/modprobe psmouse
    '';
    powertop.enable = true;
  };

  services.acpid.enable = true;
  services.thermald.enable = true;
  services.tlp.enable = true;
  services.logind.lidSwitch = "suspend";

  hardware = {
    firmware = with pkgs; [
      linux-firmware
    ];
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };
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

  # system.activationScripts.kmonad.text = "${pkgs.systemd}/bin/systemctl try-restart kmonad-laptop-internal";
  # }}}

  # networking {{{
  networking.hostName = "thonkpad";
  services.connman.enable = true;

  # jebać ose
  security.pki.certificateFiles = [
    (pkgs.fetchurl {
      url = "https://ose.gov.pl/media/2022/09/certyfikat-OSE.crt";
      sha256 = "0Du2OmOnvCzaGz3ZHu7KsnL23WjDxNfgn3VFqzf2ffA=";
    })
  ];
  # }}}
}
