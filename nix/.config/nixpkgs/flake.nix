{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    neovim-nightly-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self, darwin, nixpkgs, home-manager, neovim-nightly-overlay, ... }@inputs:

    let

      hmModule = {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          users.bandithedoge = {imports = [./home ./home/neovim];};
        };
      };

      overlays = with inputs; [ neovim-nightly-overlay.overlay ];

    in {
      darwinConfigurations."machine" = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          { nixpkgs.overlays = overlays; }
          ./darwin
          home-manager.darwinModule
          hmModule
        ];
      };
    };
}
