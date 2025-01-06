{pkgs, ...}: {
  home = {
    packages = with pkgs; [
      gpu-screen-recorder
      gpu-screen-recorder-gtk
    ];
    sessionVariables = {
      BROWSER = "floorp";
      WINEFSYNC = "1";
    };
  };

  programs.looking-glass-client = {
    enable = true;
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
      enable = false;
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
      enable = false;
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

  systemd.user.services.gpu-screen-recorder = {
    Unit = {
      Description = "GPU Screen Recorder Service";
      After = ["pipewire.service"];
    };
    Service = {
      ExecStartPre = "${pkgs.lib.getBin pkgs.libnotify}/bin/notify-send -u low 'GPU Screen Recorder' 'Replay started' -i com.dec05eba.gpu_screen_recorder -a 'GPU Screen Recorder'";
      ExecStart = pkgs.lib.concatStringsSep " " [
        "${pkgs.lib.getBin pkgs.gpu-screen-recorder}/bin/gpu-screen-recorder"
        "-w DP-2"
        "-c mp4"
        "-q ultra"
        "-cr full"
        "-f 60"
        "-r 60"
        "-o /mnt/data/recordings"
        "-ac opus"
        "-ab 320"
        "-a default_output|mono-microphone"
        "-a default_output"
        "-a mono-microphone"
      ];
      KillSignal = "SIGINT";
      Restart = "on-failure";
      RestartSec = "5";
      Type = "simple";
    };
    Install.WantedBy = ["graphical-session.target"];
  };
}
