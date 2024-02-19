{
  pkgs,
  home-manager,
  config,
  ...
}: let
  cache = config.xdg.cacheHome;
  conf = config.xdg.configHome;
  data = config.xdg.dataHome;
  state = config.xdg.stateHome;
in {
  # TODO: more stuff from xdg-ninja
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

      # cabal
      CABAL_CONFIG = "${conf}/cabal/config";
      CABAL_DIR = "${data}/cabal";

      # go
      GOPATH = "${data}/go";

      # x
      XCOMPOSECACHE = "${cache}/X11/xcompose";

      # java
      _JAVA_OPTIONS = "-Djava.util.prefs.userRoot=${conf}/java";

      # stack
      STACK_ROOT = "${data}/stack";

      # npm
      NPM_CONFIG_USERCONFIG = "${conf}/npm/npmrc";
    };

    shellAliases = {
      # wget
      wget = "wget --hsts-file=\"${data}/wget-hsts\"";

      # yarn
      yarn = "yarn --use-yarnrc ${conf}/yarn/config";
    };
  };
}
