{pkgs, ...}: {
  home.file = {
    ".mpd/mpd.conf".text = ''
      music_directory "/Volumes/data/Music/Music"

      playlist_directory "~/.mpd/playlists"
      db_file            "~/.mpd/mpd.db"
      log_file           "~/.mpd/mpd.log"
      pid_file           "~/.mpd.pid"
      state_file         "~/.mpd/mpdstate"

      audio_output {
        type                  "ao"
        name                  "CoreAudio"
      }
    '';

    "Library/Application Support/discord-rpc/config.toml".source = (pkgs.formats.toml {}).generate "config.toml" {
      id = 922175995828654100;
      format = {
        details = "$artist - $title";
        state = "$album";
        small_image = "";
        large_text = "$date";
      };
    };
  };
}
