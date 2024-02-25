{pkgs, ...}: {
  programs.virt-manager.enable = true;

  virtualisation = {
    libvirtd = {
      enable = true;
      onShutdown = "shutdown";
      onBoot = "ignore";
      qemu = {
        runAsRoot = false;
        ovmf.enable = true;
        vhostUserPackages = with pkgs; [virtiofsd];
      };
    };
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
    blacklistedKernelModules = [
      "i915"
    ];
    kernelParams = [
      "intel_iommu=on"
      "iommu=pt"
      "vfio-pci.ids=8086:0412"
      "vga=normal"
      "i915.modeset=0"
      "kvm.ignore_msrs=1"
      "kvm.report_ignored_msrs=0"
      "kvm_intel.nested=1"
      "kvm_intel.emulate_invalid_guest_state=0"
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
    settings = {
      win = {
        inherit (pkgs.rice) uiFont;
        uiSize = 16;
        fullScreen = true;
        quickSplash = true;
        jitRender = true;
      };
      input = {
        escapeKey = 135; # menu key
      };
      audio = {
        micDefault = "allow";
        periodSize = 512;
      };
      wayland.warpSupport = false;
    };
  };
}
