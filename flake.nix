{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    nixlib.url = "github:nix-community/nixpkgs.lib";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    digga = {
      url = "github:divnix/digga";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
        nixlib.follows = "nixlib";
        darwin.follows = "darwin";
      };
    };

    nur.url = "github:nix-community/NUR";
    nur-bandithedoge.url = "github:bandithedoge/nur-packages";
    nur-bandithedoge.flake = false;
    nur-bandithedoge.inputs.nixpkgs.follows = "nixpkgs";
    neorg.url = "github:nvim-neorg/nixpkgs-neorg-overlay";
    neorg.inputs.nixpkgs.follows = "nixpkgs";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    neovim-nightly-overlay.inputs.nixpkgs.follows = "nixpkgs";
    parinfer-rust.url = "github:eraserhd/parinfer-rust";
    parinfer-rust.flake = false;

    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nixpkgs-wayland.inputs.nixpkgs.follows = "nixpkgs";
    musnix.url = "github:musnix/musnix";
    kmonad.url = "github:kmonad/kmonad?dir=nix";
    nixgl.url = "github:guibou/nixGL";
    firefox-darwin.url = "github:bandithedoge/nixpkgs-firefox-darwin";
  };

  outputs = {
    self,
    digga,
    nixpkgs,
    home-manager,
    musnix,
    kmonad,
    nur,
    nixgl,
    neorg,
    parinfer-rust,
    nur-bandithedoge,
    neovim-nightly-overlay,
    ...
  } @ inputs: let
    overlays = [
      nur.overlay
      # neovim-nightly-overlay.overlay
      nixgl.overlay
      neorg.overlay
      # nixpkgs-wayland.overlay
      # (import (parinfer-rust + "/overlay.nix"))
      (_: prev: {
        bandithedoge = import nur-bandithedoge {pkgs = prev;};
      })
      (import ./overlay.nix)
    ];
  in
    digga.lib.mkFlake {
      inherit self inputs;

      sharedOverlays = overlays;

      channelsConfig = {
        allowUnfree = true;
        allowBroken = true;
      };

      channels = {
        nixpkgs = {
          inherit overlays;
          input = nixpkgs;
        };
      };

      nixos = {
        hostDefaults = {
          system = "x86_64-linux";
          channelName = "nixpkgs";
          modules = [
            digga.nixosModules.nixConfig
            home-manager.nixosModules.home-manager
            musnix.nixosModules.musnix
            kmonad.nixosModules.default
            ./nix.nix
          ];
        };

        imports = [(digga.lib.importHosts ./hosts)];

        importables = rec {
          profiles =
            digga.lib.rakeLeaves ./profiles
            // {
              users = digga.lib.rakeLeaves ./users;
            };
          suites = with profiles; {
            base = [core users.bandithedoge];
            gui = [gui];
            audio = [audio];
          };
        };

        hosts = {
          thonkpad = {};
        };
      };

      home = {
        modules = [];

        importables = rec {
          profiles = digga.lib.rakeLeaves ./users/profiles;
          suites = with profiles; {
            base = [
              core
              editors
            ];
            linux = [os-specific.linux];
            gui = [
              # wayland
              x
            ];
            audio = [
              audio
            ];
          };
        };

        users = {
          bandithedoge = {suites, ...}: {
            imports = with suites;
              base
              ++ gui
              ++ audio
              ++ linux;
          };
        };
      };

      # just wsl things
      homeConfigurations."bandithedoge" = let
        system = "x86_64-linux";
      in
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          modules = [
            {
              home = {
                homeDirectory = "/home/bandithedoge";
                username = "bandithedoge";
                stateVersion = "21.11";
              };
              nixpkgs = {inherit overlays;};
            }
            ./users/profiles/core
            ./users/profiles/editors
            ./nix.nix
          ];
        };
    };
}
