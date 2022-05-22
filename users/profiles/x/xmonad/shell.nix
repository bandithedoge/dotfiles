{pkgs ? import <nixpkgs> {config.allowBroken = true;}}:
# pkgs.haskellPackages.developPackage {
#   name = "my-xmonad";
#   root = ./.;
#   modifier = drv:
#     pkgs.haskell.lib.addBuildTools drv (with pkgs; [
#       haskell-language-server
#     ]);
# }
pkgs.mkShell {
  inputsFrom = [(pkgs.haskellPackages.callCabal2nix "my-xmonad" ./. {}).env];
}
