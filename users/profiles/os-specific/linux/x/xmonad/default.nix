{
  pkgs ? import <nixpkgs> {config.allowBroken = true;},
  xmonad-entryhelper ?
    pkgs.haskellPackages.xmonad-entryhelper.overrideAttrs (old: {
      src = pkgs.fetchFromGitHub {
        owner = "Javran";
        repo = "xmonad-entryhelper";
        rev = "ee2d0c14f9258503d7bd62907aa731dd64fa34d0";
        sha256 = "1m2hl0jnmq7kb9ix5xmnjh8jj7b7frprw244ykh3jyhq3mns4rh8";
      };
    }),
}:
pkgs.haskellPackages.callCabal2nix "my-xmonad" ./. {
  inherit xmonad-entryhelper;
}
