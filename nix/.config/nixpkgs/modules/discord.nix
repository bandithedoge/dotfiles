{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.discord;

  pluginModule = types.submodule ({ ... }: {
    options = {
      src = mkOption { type = types.path; };
      name = mkOption { type = types.str; };
    };
  });

in {
  options.programs.discord = {
    enable = mkEnableOption "betterdiscord";
    plugins = mkOption {
      type = types.listOf pluginModule;
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    home = mkIf (length cfg.plugins > 0) {
      file = mkMerge ((map (plugin: {
        "Library/Application Support/BetterDiscord/plugins/${plugin.name}.plugin.js".source =
          plugin.src;
      }) cfg.plugins));
    };
  };
}
