{
  pkgs,
  home-manager,
  config,
  ...
}: {
  home = {
    sessionVariables = {
      # android
      ANDROID_HOME = "$XDG_DATA_HOME/android";

      # bash
      HISTFILE = "$XDG_STATE_HOME/bash/history";

      # cabal
      CABAL_CONFIG = "$XDG_CONFIG_HOME/cabal/config";
      CABAL_DIR = "$XDG_DATA_HOME/cabal";

      # go
      GOPATH = "$XDG_DATA_HOME/go";

      # x
      XCOMPOSECACHE = "$XDG_CACHE_HOME/X11/xcompose";

      # java
      _JAVA_OPTIONS = "-Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java";

      # stack
      STACK_ROOT = "$XDG_DATA_HOME/stack";

      # npm
      NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/npmrc";
    };

    shellAliases = {
      # wget
      wget = "wget --hsts-file=\"$XDG_DATA_HOME/wget-hsts\"";

      # yarn
      yarn = "yarn --use-yarnrc $XDG_CONFIG_HOME/yarn/config";
    };
  };

  xdg.configFile = {
    # npm
    "npm/npmrc".text = ''
      prefix=$\{XDG_DATA_HOME}/npm
      cache=$\{XDG_CACHE_HOME}/npm
      tmp=$\{XDG_RUNTIME_DIR}/npm
      init-module=$\{XDG_CONFIG_HOME}/npm/config/npm-init.js
    '';
  };
}
