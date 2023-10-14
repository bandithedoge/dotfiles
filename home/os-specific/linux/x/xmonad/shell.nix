{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  inputsFrom = [(import ./default.nix {}).env];
  buildInputs = with pkgs; [
    haskell-language-server
  ];
}
