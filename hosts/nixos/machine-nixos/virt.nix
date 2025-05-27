{
  pkgs,
  config,
  ...
}: {
  programs.virt-manager.enable = true;

  virtualisation = {
    libvirtd = {
      enable = true;
      onShutdown = "shutdown";
      onBoot = "ignore";
      qemu = {
        # runAsRoot = false;
        ovmf.enable = true;
        vhostUserPackages = with pkgs; [virtiofsd];
        verbatimConfig = ''
          user = "bandithedoge"

          cgroup_device_acl = [
            "/dev/null", "/dev/full", "/dev/zero",
            "/dev/random", "/dev/urandom",
            "/dev/ptmx", "/dev/kvm", "/dev/kqemu",
            "/dev/rtc","/dev/hpet", "/dev/vfio/vfio",

            "/dev/kvmfr0",
            "/dev/input/by-id/usb-Logitech_G102_Prodigy_Gaming_Mouse_018438583538-event-mouse",
            "/dev/input/by-id/usb-SEMITEK_USB-HID_Gaming_Keyboard_SN0000000001-event-kbd",
          ]

          security_default_confined = 0
          clear_emulator_capabilities = 0
        '';
      };
      hooks.qemu = {
        webdav = pkgs.writeShellScript "virt-webdav-hook" ''
          if [ $1 = DarwinKVM ]; then
            case $2 in
              start)
                systemctl start virt-webdav
                ;;
              stopped)
                systemctl stop virt-webdav
                ;;
            esac
          fi
        '';
      };
    };
    # waydroid.enable = true;
  };

  users.users."qemu-libvirtd" = {
    extraGroups = ["kvm" "input"];
    isSystemUser = true;
  };

  boot = {
    kernelModules = [
      "kvm-intel"
      # "kvmfr"
      "vfio"
      "vfio_iommu_type1"
      "vfio_pci"
    ];
    extraModulePackages = with config.boot.kernelPackages; [
      # https://github.com/NixOS/nixpkgs/issues/379503
      # kvmfr
    ];
    initrd.kernelModules = [
      "vfio"
      "vfio_iommu_type1"
      "vfio_pci"
    ];
    kernelParams = [
      "intel_iommu=on"
      "iommu=pt"
      "vfio-pci.ids=1002:67df,1002:aaf0"
      "vga=normal"
      "kvm.ignore_msrs=1"
      "kvm.report_ignored_msrs=0"
      "kvm_intel.nested=1"
      "kvm_intel.emulate_invalid_guest_state=0"
      "pcie_acs_override=downstream,multifunction"
      "kvmfr.static_size_mb=32"
    ];
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="vfio", OWNER="root", GROUP="kvm"
    SUBSYSTEM=="kvmfr", OWNER="bandithedoge", GROUP="kvm", MODE="0660"
  '';

  systemd.tmpfiles.rules = [
    "f /dev/shm/looking-glass 666 bandithedoge qemu-libvirtd -"
  ];

  networking.firewall = {
    allowedUDPPortRanges = [
      {
        from = 60000;
        to = 61000;
      }
    ];
    allowedTCPPorts = [4918];
  };

  services = {
    samba = {
      enable = true;
      openFirewall = true;
      settings = {
        data = {
          path = "/mnt";
          "read only" = "no";
          "guest ok" = "yes";
          "bind interfaces only" = "yes";
          interfaces = "virbr0";
        };
        global = {
          security = "user";
          "passwd program" = "/run/wrappers/bin/passwd %u";
          "invalid users" = ["root"];
        };
      };
    };
  };

  systemd.services.virt-webdav = {
    after = ["network.target"];
    serviceConfig = {
      ExecStart = "${pkgs.rclone}/bin/rclone serve webdav /mnt --addr 192.168.122.1:4918 --bind 192.168.122.1";
      Restart = "on-failure";
    };
  };
}
