{
  pkgs,
  config,
  ...
}: let
  configPath =
    if pkgs.stdenv.isDarwin
    then "Library/Application Support"
    else config.xdg.configHome;
in {
  programs.firefox = {
    # {{{
    enable = true;
    package =
      if !pkgs.stdenv.isDarwin
      then
        pkgs.firefox-beta-bin.override
        {
          cfg.enableTridactylNative = true;
        }
      else pkgs.dummy;
    extensions = with pkgs.bandithedoge.firefoxAddons; [
      augmented-steam
      auto-tab-discard
      base64-decoder
      betterviewer
      canvasblocker
      csgofloat
      dont-fuck-with-paste
      downthemall
      enhanced-github
      enhancer-for-youtube
      gesturefy
      gitako
      github-code-folding
      github-isometric-contributions
      github-repo-size
      imagus
      lovely-forks
      material-icons-for-github
      npm-hub
      octolinker
      privacy-badger
      privacy-pass
      pronoundb
      reddit-enhancement-suite
      refined-github
      ruffle
      sourcegraph
      sponsorblock
      steam-database
      stylus
      tabcenter-reborn
      tridactyl
      ublock-origin
      violentmonkey
    ];
    profiles = {
      default = {
        name = "default";
        settings = {
          "app.update.auto" = false;
          "browser.autofocus" = false;
          "gfx.webrender.all" = true;
          "layers.acceleration.force-enabled" = true;
          "svg.context-properties.content.enabled" = true;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "ui.context_menus.after_mouseup" = true;
        };
        userChrome = ''
          #titlebar, #sidebar-header {
            display: none;
          }
        '';
      };
    };
  }; # }}}

  home.file.${configPath + "/discordcanary/settings.json"}.text = builtins.toJSON {
    # {{{
    enableHardwareAcceleration = false;
    OPEN_ON_STARTUP = false;
    openasar = {
      quickstart = true;
      setup = true;
      js = ''
        const css = `
          ${builtins.readFile (pkgs.rice.compileSCSS ./discord.scss)}
        `;
        const style = document.createElement("style");
        style.textContent = css;
        document.head.appendChild(style);
      '';
    };
  }; # }}}
}
