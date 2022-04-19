{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nur.url = "github:nix-community/NUR";
    nur-bandithedoge.url = "github:bandithedoge/nur-packages";
    nur-bandithedoge.flake = false;
    vim-extra-plugins.url = "github:m15a/nixpkgs-vim-extra-plugins";
    neorg.url = "github:nvim-neorg/nixpkgs-neorg-overlay";
    neorg.inputs.nixpkgs.follows = "nixpkgs";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    neovim-nightly-overlay.inputs.nixpkgs.follows = "nixpkgs";
    parinfer-rust.url = "github:eraserhd/parinfer-rust";
    parinfer-rust.flake = false;
    zigf.url = "github:roarkanize/zig-overlay";
    zigf.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";
    musnix.url = "github:musnix/musnix";
    kmonad.url = "github:kmonad/kmonad?dir=nix";
    nixgl.url = "github:guibou/nixGL";
    firefox-darwin.url = "github:bandithedoge/nixpkgs-firefox-darwin";
  };

  outputs =
    { self
    , darwin
    , nixpkgs
    , home-manager
    , nixos-hardware
    , musnix
    , kmonad
    , ...
    }@inputs:

    let
      hmModule = {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          users.bandithedoge = import ./home;
        };
      };

      common = {
        imports = [ ./common ];
        nix.registry = with inputs; { nixpkgs.flake = nixpkgs; };
      };

      nixpkgsConfig = {
        nixpkgs = {
          overlays = with inputs; [
            neovim-nightly-overlay.overlay
            nixgl.overlay
            vim-extra-plugins.overlay
            neorg.overlay
            (import (parinfer-rust + "/overlay.nix"))
            (import (nur-bandithedoge + "/overlay.nix"))
            (import ./overlay.nix)
            (final: prev: {
              inherit zigf;
            })
          ];
          config.packageOverrides = pkgs: {
            nur = import inputs.nur { inherit pkgs; };
          };
        };
      };

    in
    {
      darwinConfigurations."machine" = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          ./common
          ./darwin
          nixpkgsConfig
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
          ./common
          ./nixos
          nixpkgsConfig
          nixos-hardware.nixosModules.lenovo-thinkpad-t440p
          musnix.nixosModules.musnix
          home-manager.nixosModules.home-manager
          kmonad.nixosModule
          (nixpkgs.lib.mkMerge [
            hmModule
            { home-manager.users.bandithedoge.imports = [ ./home/linux ]; }
          ])
        ];
      };
      homeConfigurations.bandithedoge = home-manager.lib.homeManagerConfiguration {
        configuration = {
          imports = [ ./home ./home/wsl ];
        } // nixpkgsConfig;
        system = "x86_64-linux";
        username = "bandithedoge";
        homeDirectory = "/mnt/c/Users/bandithedoge";
      };
    };
}
