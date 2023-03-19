{pkgs ? import <nixpkgs> {}}:
pkgs.poetry2nix.mkPoetryApplication rec {
  projectDir = ./.;
  python = pkgs.python310;

  postInstall = ''
    wrapProgram $out/bin/keepass \
      --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.xdotool]}
  '';

  overrides = pkgs.poetry2nix.overrides.withDefaults (self: super: {
    python-rofi-phfn = super.python-rofi-phfn.overridePythonAttrs (old: {
      buildInputs = old.buildInputs ++ [python.pkgs.flit];
    });

    # https://github.com/NixOS/nixpkgs/issues/204494
    rofi-menu = super.rofi-menu.overridePythonAttrs (old: {
      buildInputs = old.buildInputs ++ [pkgs.poetry];
    });
  });
}
