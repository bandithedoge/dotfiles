{ home-manager, pkgs, ... }: {
  nixpkgs.overlays = [
    (self: super: {
      dwm = super.dwm.overrideAttrs (oldAttrs: rec {
        patches = [

        ];
      });
    })
  ];
}
