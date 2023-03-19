{pkgs, ...}: {
  # https://github.com/nix-community/NixOS-WSL/issues/185
  systemd.services.nixos-wsl-systemd-fix = {
    description = "Fix the /dev/shm symlink to be a mount";
    unitConfig = {
      DefaultDependencies = "no";
      Before = ["sysinit.target" "systemd-tmpfiles-setup-dev.service" "systemd-tmpfiles-setup.service" "systemd-sysctl.service"];
      ConditionPathExists = "/dev/shm";
      ConditionPathIsSymbolicLink = "/dev/shm";
      ConditionPathIsMountPoint = "/run/shm";
    };
    serviceConfig = {
      Type = "oneshot";
      ExecStart = [
        "${pkgs.coreutils-full}/bin/rm /dev/shm"
        "/run/wrappers/bin/mount --bind -o X-mount.mkdir /run/shm /dev/shm"
      ];
    };
    wantedBy = ["sysinit.target"];
  };

  wsl = {
    enable = true;
    defaultUser = "bandithedoge";
    interop.includePath = false;
    wslConf.interop = {
      enabled = false;
      appendWindowsPath = false;
    };
  };

  networking.hostName = "wsl";

  users.users.bandithedoge = {
    name = "bandithedoge";
    shell = pkgs.fish;
    uid = 1000;
    extraGroups = ["wheel"];
    isNormalUser = true;
  };

  environment.shells = with pkgs; [
    fish
    bashInteractive
  ];

  system.stateVersion = "23.05";
}
