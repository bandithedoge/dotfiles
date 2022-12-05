{pkgs, ...}: {
  services.nix-daemon.enable = true;

  programs.zsh.enable = true;
  programs.fish.enable = true;

  homebrew = {
    enable = true;
    caskArgs.no_quarantine = true;
    onActivation = {
      cleanup = "uninstall";
      upgrade = true;
      autoUpdate = true;
    };
    taps = [
      "gromgit/homebrew-fuse"
      "homebrew/cask"
      "homebrew/cask-fonts"
      "homebrew/cask-versions"
      "homebrew/services"
    ];
    brews = [
      {
        name = "mpd";
        restart_service = "changed";
      }
      "innoextract"
      "nicotine-plus"
      "nim"
      "python"
      "python-tk"
      "rclone-mac"
    ];
    casks = [
      "qlimagesize"
      "qlmarkdown"
      "qlstephen"
      "quicklook-json"
      "quicklookapk"
      "syntax-highlight"

      "android-file-transfer"
      "android-platform-tools"
      "anydesk"
      "apparency"
      "appcleaner"
      "balenaetcher"
      "blackhole-2ch"
      "blender"
      "blender-benchmark"
      "cantata"
      "discord-canary"
      "firefox-beta"
      "font-jetbrains-mono-nerd-font"
      "genymotion"
      "gzdoom"
      "iina"
      "jdownloader"
      "keepassxc"
      "kitty"
      "librewolf"
      "lulu"
      "macfuse"
      "messenger"
      "microsoft-teams"
      "mos"
      "musaicfm"
      "netnewswire"
      "nordvpn"
      "obs"
      "origin"
      "porting-kit"
      "ppsspp"
      "prismlauncher"
      "qbittorrent"
      "raze"
      "sloth"
      "stats"
      "steam"
      "suspicious-package"
      "syncthing"
      "telegram"
      "temurin"
      "vcv-rack"
      "xemu"
    ];
    masApps = {
      "Keynote" = 409183694;
      "Numbers" = 409203825;
      "Pages" = 409201541;
      "RetroClip" = 1332064978;
    };
  };

  system = {
    defaults = {
      LaunchServices.LSQuarantine = false;
      NSGlobalDomain = {
        AppleShowAllFiles = false;
        AppleInterfaceStyle = "Dark";
        AppleShowAllExtensions = true;
        AppleShowScrollBars = "Automatic";

        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;

        NSDocumentSaveNewDocumentsToCloud = false;
        NSNavPanelExpandedStateForSaveMode = true;
        NSNavPanelExpandedStateForSaveMode2 = true;

        "com.apple.swipescrolldirection" = false;
      };
      dock = {
        appswitcher-all-displays = true;
        orientation = "right";
        showhidden = true;
        show-recents = false;

        wvous-tl-corner = 2;
        wvous-tr-corner = 2;
        wvous-bl-corner = 4;
        wvous-br-corner = 4;
      };
      finder = {
        AppleShowAllFiles = false;
        ShowStatusBar = true;
        ShowPathbar = true;
        FXPreferredViewStyle = "Nlsv";
        AppleShowAllExtensions = true;
        _FXShowPosixPathInTitle = true;
      };
      loginwindow.GuestEnabled = false;
    };
  };

  launchd.user.agents.mpd-discord-rpc = {
    serviceConfig = {
      ProgramArguments = ["${pkgs.mpd-discord-rpc}/bin/mpd-discord-rpc"];
      KeepAlive = true;
      RunAtLoad = true;
      ThrottleInterval = 30;
    };
  };

  services.skhd = {
    enable = true;
    skhdConfig = ''
      play : ${pkgs.mpc-cli}/bin/mpc toggle
      previous : ${pkgs.mpc-cli}/bin/mpc prev
      next : ${pkgs.mpc-cli}/bin/mpc next
    '';
  };
}
