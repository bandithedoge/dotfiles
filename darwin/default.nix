{ config, pkgs, inputs, ... }:

{
  services.nix-daemon.enable = true;
  environment.darwinConfig = ./.;

  networking = {
    hostName = "machine";
    localHostName = "machine";
    computerName = "Double Quarter Pounder with Cheese";

    knownNetworkServices = [ "Ethernet" ];
    dns = [ "1.1.1.1" "1.0.0.1" "2606:4700:4700::1111" "2606:4700:4700::1001" ];
  };

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
    autoUpdate = true;
    cleanup = "zap";

    taps = [
      "homebrew/cask"
      "homebrew/cask-fonts"
      "homebrew/cask-versions"
      "gromgit/fuse"
      "tblock/tap"
    ];

    brews = [ "libusb" "innoextract" "mpd" "ntfs-3g-mac" "svn" "trash" ];

    casks = [
      "android-platform-tools"
      "anydesk"
      "apparency"
      "appcleaner"
      "audio-hijack"
      "balenaetcher"
      "betterdiscord-installer"
      "blackhole-2ch"
      "blender"
      "cantata"
      "discord-canary"
      "eloston-chromium"
      "firefox-beta"
      "font-jetbrains-mono-nerd-font"
      "font-roboto"
      "iina"
      "iterm2"
      "jdownloader"
      "kapitainsky-rclone-browser"
      "keepassxc"
      "kitty"
      "loopback"
      "lulu"
      "macfuse"
      "macpass"
      "meld"
      "microsoft-teams"
      "milkytracker"
      "mos"
      "multimc"
      "musicbrainz-picard"
      "nfov"
      "obs"
      "obs-virtualcam"
      "openemu"
      "openmw"
      "prusaslicer"
      "qlcolorcode"
      "qlcommonmark"
      "qlimagesize"
      "qlstephen"
      "quicklook-json"
      "quicklookapk"
      "quicknfo"
      "safari-technology-preview"
      "schism-tracker"
      "soulseek"
      "steam"
      "suspicious-package"
      "syncthing"
      "telegram"
      "temurin"
      "temurin8"
      "the-unarchiver"
      "transmission"
      "vamiga"
      "vcv-rack"
      "vscodium"
      "xbar"
    ];

    masApps = {
      "Messenger" = 1480068668;
      "Pages" = 409201541;
      "RetroClip" = 1332064978;
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

  system.defaults = {
    LaunchServices.LSQuarantine = false;

    NSGlobalDomain = {
      AppleInterfaceStyle = "Dark";
      AppleShowAllExtensions = true;
      NSAutomaticCapitalizationEnabled = false;
      NSAutomaticSpellingCorrectionEnabled = false;
      NSNavPanelExpandedStateForSaveMode = true;
      NSNavPanelExpandedStateForSaveMode2 = true;
    };

    dock = {
      orientation = "right";
      show-recents = false;
      showhidden = true;
    };

    finder = {
      AppleShowAllExtensions = true;
      QuitMenuItem = true;
      _FXShowPosixPathInTitle = true;
    };
  };
}
