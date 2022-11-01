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
    ];
    brews = [
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

      "anydesk"
      "apparency"
      "appcleaner"
      "balenaetcher"
      "blackhole-2ch"
      "discord-canary"
      "firefox-beta"
      "font-jetbrains-mono-nerd-font"
      "iina"
      "jdownloader"
      "keepassxc"
      "kitty"
      "librewolf"
      "lulu"
      "macfuse"
      "messenger"
      "mos"
      "musaicfm"
      "netnewswire"
      "obs"
      "qbittorrent"
      "sloth"
      "stats"
      "strawberry"
      "suspicious-package"
      "syncthing"
      "telegram"
      "the-unarchiver"
      "tunnelblick"
      "utm"
    ];
  };

  system = {
    defaults = {
      LaunchServices.LSQuarantine = false;
      NSGlobalDomain = {
        AppleShowAllFiles = true;
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
        AppleShowAllFiles = true;
        ShowStatusBar = true;
        ShowPathbar = true;
        FXPreferredViewStyle = "Nlsv";
        AppleShowAllExtensions = true;
        _FXShowPosixPathInTitle = true;
      };
      loginwindow.GuestEnabled = false;
    };
  };
}
