{pkgs, ...}: {
  system.stateVersion = "24.11";

  hardware.enableRedistributableFirmware = true;

  environment = {
    systemPackages = with pkgs; [
      alsa-utils
      ntfs3g
    ];
  };
  services = {
    dbus = {
      enable = true;
      packages = with pkgs; [dconf];
    };

    printing = {
      enable = true;
      cups-pdf.enable = true;
      drivers = with pkgs; [
        gutenprint
        cnijfilter2
      ];
    };

    earlyoom = {
      enable = true;
      enableNotifications = true;
      freeMemThreshold = 5;
      freeSwapThreshold = 5;
    };

    # scx = {
    #   enable = true;
    #   scheduler = "scx_bpfland";
    #   package = pkgs.scx_git.full;
    # };

    ananicy = {
      enable = true;
      package = pkgs.ananicy-cpp;
      rulesProvider = pkgs.ananicy-rules-cachyos_git;
    };

    devmon.enable = true;
    irqbalance.enable = true;
    openssh.enable = true;
    resolved.enable = true;
    upower.enable = true;
  };

  programs = {
    java = {
      enable = true;
      package = pkgs.temurin-jre-bin;
      binfmt = true;
    };

    adb.enable = true;
    ccache.enable = true;
    dconf.enable = true;
    nix-ld.enable = true;
    udevil.enable = true;
  };

  security = {
    rtkit.enable = true;
    pam.services.gtklock.text = ''
      auth include login
    '';
    sudo.enable = false;
    sudo-rs.enable = true;
  };

  users.mutableUsers = true;

  time.timeZone = "Europe/Warsaw";

  console = {
    colors = map (pkgs.lib.removePrefix "#") (with pkgs.rice; [
      base00
      base08
      base0B
      base09
      base0D
      base0E
      base0C
      base06
      base02
      base12
      base14
      base13
      base16
      base17
      base15
      base0F
    ]);
    keyMap = "pl";
  };

  boot = {
    tmp.cleanOnBoot = true;
    # https://github.com/CachyOS/CachyOS-Settings/blob/master/usr/lib/sysctl.d/99-cachyos-settings.conf
    kernel.sysctl = {
      "fs.file-max" = 2097152;
      "fs.inotify.max_user_watches" = 524288;
      "fs.xfs.xfssyncd_centisecs" = 10000;
      "kernel.kexec_load_disabled" = true;
      "kernel.kptr_restrict" = 2;
      "kernel.nmi_watchdog" = 0;
      "kernel.printk" = "3 3 3 3";
      "kernel.sched_rt_runtime_us" = 980000;
      "kernel.split_lock_mitigate" = 0;
      "kernel.unprivileged_userns_clone" = true;
      "net.core.netdev_max_backlog" = 16384;
      "net.core.somaxconn" = 8192;
      "net.ipv4.tcp_congestion_control" = "bbr";
      "net.ipv4.tcp_ecn" = 1;
      "net.ipv4.tcp_fastopen" = 3;
      "net.ipv4.tcp_rfc1337" = true;
      "net.ipv4.tcp_slow_start_after_idle" = false;
      "net.ipv4.tcp_syncookies" = true;
      "net.ipv4.tcp_timestamps" = false;
      "vm.dirty_writeback_centisecs" = 1500;
      "vm.page-cluster" = 1;
      "vm.swappiness" = pkgs.lib.mkForce 40;
    };
  };

  zramSwap.enable = true;

  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    defaultNetwork.settings.dns_enabled = true;
  };

  systemd.coredump.extraConfig = ''
    Storage=none
    ProcessSizeMax=0
  '';
}
