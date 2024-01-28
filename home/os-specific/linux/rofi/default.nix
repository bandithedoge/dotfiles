{
  pkgs ? import <nixpkgs> {
    overlays = [(builtins.getFlake "github:nix-community/poetry2nix").overlays.default];
  },
}:
pkgs.poetry2nix.mkPoetryApplication rec {
  projectDir = ./.;
  python = pkgs.python311;

  postInstall = ''
    wrapProgram $out/bin/keepass \
      --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.wtype]}
  '';

  overrides = pkgs.poetry2nix.overrides.withDefaults (self: super: {
    python-rofi-phfn = super.python-rofi-phfn.overridePythonAttrs (old: {
      buildInputs = old.buildInputs ++ [python.pkgs.flit];
    });

    # https://github.com/NixOS/nixpkgs/issues/204494
    rofi-menu = super.rofi-menu.overridePythonAttrs (old: {
      buildInputs = old.buildInputs ++ [python.pkgs.poetry-core];
      postPatch = ''
        substituteInPlace pyproject.toml --replace 'poetry.masonry.api' 'poetry.core.masonry.api'
      '';
    });
  });
}
