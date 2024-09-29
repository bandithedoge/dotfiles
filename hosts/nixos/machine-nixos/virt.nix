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
      "vfio"
      "vfio_iommu_type1"
      "vfio_pci"
      "vfio_virqfd"
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
    ];
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="vfio", OWNER="root", GROUP="kvm"
  '';

  systemd.tmpfiles.rules = [
    "f /dev/shm/looking-glass 666 bandithedoge qemu-libvirtd -"
  ];

  home-manager.users.bandithedoge.programs.looking-glass-client = {
    enable = true;
    package = pkgs.looking-glass-client.overrideAttrs (_: {
      version = "B6";
      src = pkgs.fetchFromGitHub {
        owner = "gnif";
        repo = "LookingGlass";
        rev = "B6";
        sha256 = "sha256-6vYbNmNJBCoU23nVculac24tHqH7F4AZVftIjL93WJU=";
        fetchSubmodules = true;
      };
      patches = [];
    });
    settings = {
      win = {
        inherit (pkgs.rice) uiFont;
        uiSize = 16;
        fullScreen = true;
        quickSplash = true;
        # jitRender = true;
      };
      # input = {
      #   escapeKey = "KEY_HOME";
      # };
      audio = {
        micDefault = "allow";
        periodSize = 512;
      };
    };
  };

  networking.firewall = {
    allowedUDPPortRanges = [
      {
        from = 60000;
        to = 61000;
      }
    ];
    allowedTCPPorts = [4918];
  };

  # services.samba = {
  #   enable = true;
  #   openFirewall = true;
  #   shares.data = {
  #     path = "/mnt";
  #     "read only" = false;
  #     "guest ok" = "yes";
  #   };
  #   extraConfig = ''
  #     bind interfaces only = yes
  #     interfaces = virbr0
  #   '';
  # };

  systemd.services.virt-webdav = {
    after = ["network.target"];
    serviceConfig = {
      ExecStart = "${pkgs.rclone}/bin/rclone serve webdav /mnt --addr 192.168.122.1:4918 --bind 192.168.122.1";
      Restart = "on-failure";
    };
  };
}
