{ pkgs, inputs, home-manager, ... }:
let rice = import ../../rice.nix;
in {
  # discord {{{
  home.file.${
    (if pkgs.stdenv.isDarwin then
      "Library/Application Support/"
    else
      home-manager.cfg.configHome) + "discordcanary/settings.json"
  }.text = builtins.toJSON {
    enableHardwareAcceleration = false;
    SKIP_HOST_UPDATE = true;
    UPDATE_ENDPOINT = "https://updates.goosemod.com/goosemod";
    NEW_UPDATE_ENDPOINT = "https://updates.goosemod.com/goosemod";
  };
  home.file.${
    (if pkgs.stdenv.isDarwin then
      "Library/Application Support/"
    else
      home-manager.cfg.configHome) + "BetterDiscord/data/canary/custom.css"
  }.text = with rice;
    ''
      @import url(https://mr-miner1.github.io/Better-Badges/src/badges.css);
      @import url(https://mwittrien.github.io/BetterDiscordAddons/Themes/SettingsModal/SettingsModal.css);
      @import url(https://nyri4.github.io/Discolored/main.css);
      @import url(https://raw.githubusercontent.com/mr-miner1/cooler-activity-status/main/theme%5Bbd%5D.theme.css)
      @import url(https://fonts.googleapis.com/css2?family=${
        with pkgs.lib.strings;
        concatStringsSep "+" (splitString " " uiFont)
      });

      :root {
        /* animated badges {{{ */
        --badge-size: 22px;
        --badge-margin: 1.5px;
        --badges-everywhere-size: 15px;
        /* }}} */

        /* settings modal {{{ */
        --settingsmodalbackground: transparent;
        --settingsmodalwidth: 960px;
        --settingsmodalheight: 80vh;
        /* }}} */
      }
    '' + builtins.readFile ./discord.css;
  # }}}
}
