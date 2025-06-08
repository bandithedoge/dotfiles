{
  pkgs,
  config,
  ...
}:
{
  home = {
    packages = with pkgs; [
      cantata
      mpc
      wireguard-tools
    ];
    sessionVariables.BROWSER = "librewolf";
  };

  programs.looking-glass-client = {
    enable = false;
    # package = pkgs.looking-glass-client.overrideAttrs (_: {
    #   version = "B6";
    #   src = pkgs.fetchFromGitHub {
    #     owner = "gnif";
    #     repo = "LookingGlass";
    #     rev = "B6";
    #     sha256 = "sha256-6vYbNmNJBCoU23nVculac24tHqH7F4AZVftIjL93WJU=";
    #     fetchSubmodules = true;
    #   };
    #   patches = [];
    # });
    settings = {
      app = {
        shmFile = "/dev/kvmfr0";
      };
      win = {
        inherit (pkgs.rice) uiFont;
        uiSize = 16;
        fullScreen = true;
        quickSplash = true;
        jitRender = true;
      };
      input = {
        escapeKey = "KEY_COMPOSE";
      };
      spice = {
        audio = false;
      };
    };
  };

  services = {
    mpd = {
      enable = true;
      musicDirectory = "/mnt/data/Music/Music";
      extraConfig = ''
        audio_output {
          type "pipewire"
          name "default"
        }
      '';
      network.startWhenNeeded = true;
    };

    mpd-discord-rpc = {
      enable = true;
      settings = {
        format = {
          details = "$artist - $title";
          state = "$album";
          large_text = "$date";
          small_image = "";
        };
      };
    };
  };

  programs.beets = {
    enable = true;
    package = pkgs.beets.override {
      pluginOverrides =
        pkgs.lib.recursiveUpdate
          (pkgs.lib.genAttrs config.programs.beets.settings.plugins (name: {
            enable = true;
          }))
          {
            filetote.propagatedBuildInputs = [ pkgs.beetsPackages.filetote ];
            describe.propagatedBuildInputs = [ pkgs.bandithedoge.beetsPackages.describe ];
            yearfixer.propagatedBuildInputs = [ pkgs.bandithedoge.beetsPackages.yearfixer ];
          };
    };
    mpdIntegration.enableUpdate = true;
    settings = rec {
      directory = config.services.mpd.musicDirectory;
      library = directory + "/beets.db";
      plugins = [
        "badfiles"
        "describe"
        "discogs"
        "duplicates"
        "edit"
        "embedart"
        "fetchart"
        "info"
        "lastgenre"
        "lastimport"
        "lyrics"
        "mbsubmit"
        "mbsync"
        "missing"
        "parentwork"
        "random"
        "replaygain"
        "scrub"
        "the"
        "unimported"
        "yearfixer"
        "zero"
      ];
      original_date = true;
      per_disc_numbering = true;

      import = {
        bell = true;
        incremental = true;
        duplicate_verbose_prompt = true;
      };

      musicbrainz.external_ids = {
        discogs = true;
        spotify = true;
        bandcamp = true;
        beatport = true;
        deezer = true;
        tidal = true;
      };

      match.preferred.original_year = true;

      paths = {
        default = "%the{$albumartist}/[$year] $albumartist - $album%aunique{}/$disc-$track - $artist - $title";
        comp = "Various Artists/[$year] $album%aunique{}/$disc-$track - $artist - $title";
      };

      embedart.auto = false;

      lastgenre = {
        # canonical = false;
        count = 3;
      };

      lastfm.user = "bandithedoge";

      lyrics = {
        force = true;
        synced = true;
        sources = [
          "lrclib"
          "genius"
        ];
      };

      replaygain.backend = "ffmpeg";

      unimported.ignore_extensions = [
        "db"
        "stignore"
      ];

      zero.fields = ["albumartist_sort" "albumartists_sort"];
    };
  };
}
