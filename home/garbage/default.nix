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
  }.text = with rice; ''
    @import url(https://discordstyles.github.io/RadialStatus/dist/RadialStatus.css);

    :root {
      --rs-small-spacing: 2px;
      --rs-medium-spacing: 3px;
      --rs-large-spacing: 4px;

      --rs-small-width: 2px;
      --rs-medium-width: 3px;
      --rs-large-width: 4px;

      --rs-avatar-shape: 50%;

      --rs-online-color: ${base0B};
      --rs-idle-color: ${base0A};
      --rs-dnd-color: ${base08};
      --rs-offline-color: ${base02};
      --rs-streaming-color: ${base0E};
      --rs-invisible-color: ${base02};
      --rs-phone-color: var(--rs-online-color);

      --rs-phone-visible: block;
    }

    .compact-T3H92H .headerText-3Uvj1Y > span::before {
      content: ""; 
      width: calc(100% + 6px); 
      height: 100%;
      border-radius: 5px;
      background-color: currentColor; 
      opacity: 0.3; 
      position: absolute; 
      top: 0; 
      left: -4px;
      z-index: -1;
    }

    .compact-T3H92H .headerText-3Uvj1Y > span::after {
      content: ":";
    }
  '';
  # }}}
}
