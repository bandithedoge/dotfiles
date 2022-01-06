{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
    nix-doom-emacs.inputs.emacs-overlay.follows = "emacs-overlay";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, nixpkgs, home-manager, nixos-hardware, ... }@inputs:
    let
      hmModule = {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          users.bandithedoge = {
            imports = with inputs; [ ./home nix-doom-emacs.hmModule ];
          };
        };
      };

      overlays = with inputs; [
        (self: super: { dummy = super.hello; })
        nur.overlay
        emacs-overlay.overlay
      ];

    in {
      darwinConfigurations."machine" = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          { nixpkgs.overlays = overlays; }
          ./common
          ./darwin
          home-manager.darwinModule
          (nixpkgs.lib.mkMerge [
            hmModule
            { home-manager.users.bandithedoge.imports = [ ./home/darwin ]; }
          ])
        ];
      };
      nixosConfigurations.thonkpad = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        extraArgs = { inherit inputs; };
        modules = [
          { nixpkgs.overlays = overlays; }
          ./common
          ./nixos
          nixos-hardware.nixosModules.lenovo-thinkpad-t440p
          home-manager.nixosModules.home-manager
          (nixpkgs.lib.mkMerge [
            hmModule
            { home-manager.users.bandithedoge.imports = [ ./home/linux ]; }
          ])
        ];
      };
    };
}
