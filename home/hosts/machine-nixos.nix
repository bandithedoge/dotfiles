{pkgs, ...}: {
  home = {
    packages = with pkgs; [
      bandithedoge.cantata
      librewolf-wayland
      mpc
      wireguard-tools
    ];
    sessionVariables.BROWSER = "floorp";
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
}
