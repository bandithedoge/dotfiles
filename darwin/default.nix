{ config, pkgs, inputs, ... }:

{
  users.users."bandithedoge" = { name = "bandithedoge";};
  services.nix-daemon.enable = true;
  environment.darwinConfig = ../darwin;
  environment.shells = [
    "/bin/zsh"
    pkgs.fish
    pkgs.bashInteractive
  ];

  programs.fish.enable = true;

  services.skhd = {
    enable = true;
    skhdConfig = ''
      play : ${pkgs.mpc_cli}/bin/mpc toggle
      previous : ${pkgs.mpc_cli}/bin/mpc prev
      next : ${pkgs.mpc_cli}/bin/mpc next
    '';
  };

  homebrew = {
    enable = true;
    cleanup = "zap";

    taps = [
      "homebrew/cask"
      "homebrew/cask-fonts"
      "homebrew/cask-versions"
      "gromgit/fuse"
      "tblock/tap"
    ];

    brews = [ "innoextract" "mpd" "ntfs-3g-mac" ];

    casks = [
      "adoptopenjdk"
      "android-platform-tools"
      "anydesk"
      "apparency"
      "appcleaner"
      "audio-hijack"
      "balenaetcher"
      "betterdiscord-installer"
      "blackhole-2ch"
      "cantata"
      "cyberduck"
      "discord-canary"
      "eloston-chromium"
      "font-fira-code-nerd-font"
      "iina"
      "iterm2"
      "jdownloader"
      "kapitainsky-rclone-browser"
      "keepassxc"
      "loopback"
      "lulu"
      "macfuse"
      "macpass"
      "meld"
      "microsoft-teams"
      "mos"
      "multimc"
      "musicbrainz-picard"
      "nfov"
      "prusaslicer"
      "qlcolorcode"
      "qlcommonmark"
      "qlimagesize"
      "qlstephen"
      "quicklook-json"
      "quicklookapk"
      "quicknfo"
      "soulseek"
      "steam"
      "strawberry"
      "suspicious-package"
      "syncthing"
      "telegram"
      "the-unarchiver"
      "transmission"
      "xbar"
      "vscodium"
    ];

    masApps = {
      "Numbers" = 409203825;
      "Messenger" = 1480068668;
      "Pages" = 409201541;
      "RetroClip" = 1332064978;
      "Keynote" = 409183694;
    };
  };

  launchd.user.agents.rclone = {
    path = [ config.environment.systemPath ];
    serviceConfig.ProgramArguments = [
      "${pkgs.rclone}/bin/rclone"
      "mount"
      "shit:"
      "/Users/Shared/drive"
      "--vfs-cache-mode"
      "writes"
    ];
    serviceConfig.KeepAlive = true;
    serviceConfig.ProcessType = "Interactive";
  };
}
