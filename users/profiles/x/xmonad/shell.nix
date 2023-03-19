{pkgs ? import <nixpkgs> {}}:
# pkgs.haskell.lib.buildStackProject {
#   name = "my-xmonad";
#   inherit (pkgs.haskell.packages.ghc8107Binary) ghc;
#   buildInputs = with pkgs; [
#     git
#     pkgconfig
#     xorg.libX11
#     xorg.libXScrnSaver
#     xorg.libXext
#     xorg.libXft
#     xorg.libXinerama
#     xorg.libXrandr
#     xorg.libXrender
#     gobject-introspection
#     # cairo
#     # harfbuzz
#     gtk3
#     libdbusmenu
#     libdbusmenu-gtk3
#     gdk-pixbuf
#     # pango
#   ];
# }
pkgs.mkShell {
  inputsFrom = [(import ./default.nix {}).env];
  buildInputs = with pkgs; [
    haskell-language-server
  ];
}
