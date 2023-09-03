{pkgs, ...}: {
  # https://github.com/nix-community/NixOS-WSL/issues/185
  systemd.services = {
    nixos-wsl-systemd-fix = {
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

    mount-home-stuff = {
      unitConfig.After = ["mnt-c.mount" "mnt-e.mount"];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = [
          "${pkgs.coreutils-full}/bin/mkdir -p /home/bandithedoge/dotfiles /home/bandithedoge/git /home/bandithedoge/sql"
          "/run/wrappers/bin/mount --bind /mnt/c/Users/bandithedoge/dotfiles /home/bandithedoge/dotfiles"
          "/run/wrappers/bin/mount --bind /mnt/e/git /home/bandithedoge/git"
          "/run/wrappers/bin/mount --bind /mnt/e/sql /home/bandithedoge/sql"
        ];
      };
      wantedBy = ["sysinit.target"];
    };
  };

  wsl = {
    enable = true;
    defaultUser = "bandithedoge";
  };

  users.users.bandithedoge = {
    name = "bandithedoge";
    shell = pkgs.fish;
    uid = 1000;
    extraGroups = ["wheel"];
    isNormalUser = true;
  };

  environment = {
    systemPackages = with pkgs; [
      wslu
    ];
    shells = with pkgs; [
      fish
      bashInteractive
    ];
  };

  programs.fish.enable = true;

  system.stateVersion = "23.05";
}
