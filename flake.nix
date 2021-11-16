{
  nixConfig = {
    extra-substituters = "https://nix-community.cachix.org";
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    neovim-nightly-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, nixpkgs, home-manager, ... }@inputs:

    let

      hmModule = {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          users.bandithedoge = { imports = [ ./home ]; };
        };
      };

      overlays = with inputs; [ neovim-nightly-overlay.overlay ];

    in {
      darwinConfigurations."machine" = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          { nixpkgs.overlays = overlays; }
          ./common
          ./darwin
          home-manager.darwinModule
          hmModule
        ];
      };
    };
}
