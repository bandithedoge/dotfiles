{config, ...}: let
  cache = config.xdg.cacheHome;
  conf = config.xdg.configHome;
  data = config.xdg.dataHome;
  state = config.xdg.stateHome;
in {
  xdg = {
    enable = true;
    configFile = {
      "npm/npmrc".text = ''
        prefix=${data}/npm
        cache=${cache}/npm
        tmp=$\{XDG_RUNTIME_DIR}/npm
        init-module=${conf}/npm/config/npm-init.js
      '';
    };
  };

  home = {
    preferXdgDirectories = true;

    sessionVariables = {
      ANDROID_USER_HOME = "${data}/android";
      CARGO_HOME = "${data}/cargo";
      CIN_CONFIG = "${conf}/bcast5";
      DOTNET_CLI_HOME = "${data}/dotnet";
      HISTFILE = "${state}/bash/history";
      JULIA_DEPOT_PATH = "${data}/julia:$JULIA_DEPOT_PATH";
      NIMBLE_DIR = "${data}/nimble";
      NPM_CONFIG_USERCONFIG = "${conf}/npm/npmrc";
      XCOMPOSECACHE = "${cache}/X11/xcompose";
      _JAVA_OPTIONS = "-Djava.util.prefs.userRoot=${conf}/java";
      GNUPGHOME = "${data}/gnupg";
    };

    shellAliases = {
      wget = "wget --hsts-file=\"${data}/wget-hsts\"";
      yarn = "yarn --use-yarnrc ${conf}/yarn/config";
      adb = "HOME=\"$XDG_DATA_HOME/android\" adb";
    };
  };

  gtk.gtk2.configLocation = "${conf}/gtk-2.0/gtkrc";
}
