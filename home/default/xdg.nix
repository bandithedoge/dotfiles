{config, ...}: let
  cache = config.xdg.cacheHome;
  conf = config.xdg.configHome;
  data = config.xdg.dataHome;
  state = config.xdg.stateHome;
in {
  xdg = {
    enable = true;
    configFile = {
      # npm
      "npm/npmrc".text = ''
        prefix=${data}/npm
        cache=${cache}/npm
        tmp=$\{XDG_RUNTIME_DIR}/npm
        init-module=${conf}/npm/config/npm-init.js
      '';
    };
  };

  home = {
    sessionVariables = {
      # android
      ANDROID_USER_HOME = "${data}/android";

      # bash
      HISTFILE = "${state}/bash/history";

      # x
      XCOMPOSECACHE = "${cache}/X11/xcompose";

      # java
      _JAVA_OPTIONS = "-Djava.util.prefs.userRoot=${conf}/java";

      # npm
      NPM_CONFIG_USERCONFIG = "${conf}/npm/npmrc";

      # cinelerra
      CIN_CONFIG = "${conf}/bcast5";

      # cargo
      CARGO_HOME = "${data}/cargo";

      # nimble
      NIMBLE_DIR = "${data}/nimble";
    };

    shellAliases = {
      # wget
      wget = "wget --hsts-file=\"${data}/wget-hsts\"";

      # yarn
      yarn = "yarn --use-yarnrc ${conf}/yarn/config";

      # adb
      adb = "HOME='${data}'/android adb";
    };
  };

  # gtk
  gtk.gtk2.configLocation = "${conf}/gtk-2.0/gtkrc";
}
