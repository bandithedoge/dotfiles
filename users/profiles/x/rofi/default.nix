{ pkgs ? import <nixpkgs> { } }:
pkgs.poetry2nix.mkPoetryApplication rec {
  projectDir = ./.;
  python = pkgs.python310;

  postInstall = ''
    wrapProgram $out/bin/keepass \
      --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.xdotool]}
  '';

  overrides = pkgs.poetry2nix.overrides.withDefaults (self: super: {
    python-rofi-phfn = super.python-rofi-phfn.overridePythonAttrs (old: {
      buildInputs = old.buildInputs ++ [ python.pkgs.flit ];
    });

    rofi-menu = super.rofi-menu.overridePythonAttrs (old: {
      buildInputs = old.buildInputs ++ [ python.pkgs.poetry ];
    });
  });
}
