{
  nur ? import (import ./nix/sources.nix {}).nur-packages {},
  pkgs ?
    import <nixpkgs> {
      config.allowBroken = true;
    },
  taffybar ? nur.taffybar,
  xmonad-entryhelper ? nur.xmonad-entryhelper,
}:
pkgs.haskellPackages.callCabal2nix "my-xmonad" ./. {
  inherit taffybar xmonad-entryhelper;
}
